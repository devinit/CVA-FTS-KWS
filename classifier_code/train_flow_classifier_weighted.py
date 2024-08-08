# ! pip install datasets evaluate transformers accelerate huggingface_hub --quiet

# from huggingface_hub import login

# login()

import types
from datasets import load_dataset, Dataset
from transformers import (
    AutoTokenizer,
    AutoModelForSequenceClassification,
    DataCollatorWithPadding,
    TrainingArguments,
    Trainer
)
import torch
from torch.nn import BCEWithLogitsLoss, CrossEntropyLoss, MSELoss
from transformers.modeling_outputs import SequenceClassifierOutput
import evaluate
import numpy as np
import pandas as pd
from collections import Counter

from typing import Optional, Tuple, Union


card = "alex-miller/ODABert"
tokenizer = AutoTokenizer.from_pretrained(card, model_max_length=512)
data_collator = DataCollatorWithPadding(tokenizer=tokenizer)


def weighted_forward_bert(
    self,
    input_ids: Optional[torch.Tensor] = None,
    attention_mask: Optional[torch.Tensor] = None,
    token_type_ids: Optional[torch.Tensor] = None,
    position_ids: Optional[torch.Tensor] = None,
    head_mask: Optional[torch.Tensor] = None,
    inputs_embeds: Optional[torch.Tensor] = None,
    labels: Optional[torch.Tensor] = None,
    output_attentions: Optional[bool] = None,
    output_hidden_states: Optional[bool] = None,
    return_dict: Optional[bool] = None,
) -> Union[Tuple[torch.Tensor], SequenceClassifierOutput]:
    r"""
    labels (`torch.LongTensor` of shape `(batch_size,)`, *optional*):
        Labels for computing the sequence classification/regression loss. Indices should be in `[0, ...,
        config.num_labels - 1]`. If `config.num_labels == 1` a regression loss is computed (Mean-Square loss), If
        `config.num_labels > 1` a classification loss is computed (Cross-Entropy).
    """
    return_dict = return_dict if return_dict is not None else self.config.use_return_dict

    outputs = self.bert(
        input_ids,
        attention_mask=attention_mask,
        token_type_ids=token_type_ids,
        position_ids=position_ids,
        head_mask=head_mask,
        inputs_embeds=inputs_embeds,
        output_attentions=output_attentions,
        output_hidden_states=output_hidden_states,
        return_dict=return_dict,
    )

    pooled_output = outputs[1]

    pooled_output = self.dropout(pooled_output)
    logits = self.classifier(pooled_output)

    loss = None
    if labels is not None:
        if self.config.problem_type is None:
            if self.num_labels == 1:
                self.config.problem_type = "regression"
            elif self.num_labels > 1 and (labels.dtype == torch.long or labels.dtype == torch.int):
                self.config.problem_type = "single_label_classification"
            else:
                self.config.problem_type = "multi_label_classification"

        if self.config.problem_type == "regression":
            loss_fct = MSELoss()
            if self.num_labels == 1:
                loss = loss_fct(logits.squeeze(), labels.squeeze())
            else:
                loss = loss_fct(logits, labels)
        elif self.config.problem_type == "single_label_classification":
            loss_fct = CrossEntropyLoss(weight=self.class_weights)
            loss = loss_fct(logits.view(-1, self.num_labels), labels.view(-1))
        elif self.config.problem_type == "multi_label_classification":
            loss_fct = BCEWithLogitsLoss(pos_weight=self.class_weights)
            loss = loss_fct(logits, labels)
    if not return_dict:
        output = (logits,) + outputs[2:]
        return ((loss,) + output) if loss is not None else output

    return SequenceClassifierOutput(
        loss=loss,
        logits=logits,
        hidden_states=outputs.hidden_states,
        attentions=outputs.attentions,
    )

dataset = load_dataset("csv", data_files="CVA_flow_descriptions.csv", split="train")

# De-duplicate
df = pd.DataFrame(dataset)
print(df.shape)
df = df.drop_duplicates(subset=['text'])
print(df.shape)
dataset = Dataset.from_pandas(df, preserve_index=False)

# Add removable column to stratify
count = Counter()
count.update(dataset['label'])
print(count)

# Stratify and split
dataset = dataset.add_column('class_label', dataset['label'])
dataset = dataset.class_encode_column('class_label').train_test_split(
    test_size=0.2,
    stratify_by_column="class_label",
    shuffle=True,
    seed=42
)
dataset.remove_columns("class_label")

unique_labels = [
    "Partial",
    "Full"
]
id2label = {i: label for i, label in enumerate(unique_labels)}
label2id = {id2label[i]: i for i in id2label.keys()}

def preprocess_function(examples):
    return tokenizer(examples['text'], truncation=True)

dataset = dataset.map(preprocess_function, remove_columns=['id'])

weight_list = list()
total_rows = dataset['train'].num_rows + dataset['test'].num_rows
print("Weights:")
for label in unique_labels:
    label_idx = label2id[label]
    positive_filtered_dataset = dataset.filter(lambda example: example['label'] == label_idx)
    pos_label_rows = positive_filtered_dataset['train'].num_rows + positive_filtered_dataset['test'].num_rows
    label_weight = total_rows / pos_label_rows
    weight_list.append(label_weight)
    print("{}: {}".format(label, label_weight))

device = "cuda:0" if torch.cuda.is_available() else "cpu"
weights = torch.tensor(weight_list)
weights = weights.to(device)


clf_metrics = evaluate.combine(["accuracy", "f1", "precision", "recall"])
def compute_metrics(eval_pred):
    predictions, labels = eval_pred
    predictions = np.argmax(predictions, axis=1)
    return clf_metrics.compute(predictions=predictions, references=labels)


model = AutoModelForSequenceClassification.from_pretrained(
    card,
    num_labels=len(id2label.keys()), 
    id2label=id2label,
    label2id=label2id
)
model.forward = types.MethodType(weighted_forward_bert, model)
model.class_weights = weights

training_args = TrainingArguments(
    'cva-flow-weighted-classifier',
    learning_rate=1e-4, # This can be tweaked depending on how loss progresses
    per_device_train_batch_size=64, # These should be tweaked to match GPU VRAM
    per_device_eval_batch_size=64,
    num_train_epochs=10,
    weight_decay=0.01,
    evaluation_strategy='epoch',
    save_strategy='epoch',
    logging_strategy='epoch',
    load_best_model_at_end=True,
    push_to_hub=False,
    save_total_limit=5,
)

trainer = Trainer(
    model=model,
    args=training_args,
    train_dataset=dataset['train'],
    eval_dataset=dataset['test'],
    tokenizer=tokenizer,
    data_collator=data_collator,
    compute_metrics=compute_metrics,
)

trainer.train()
trainer.push_to_hub()
