from transformers import AutoModelForSequenceClassification, AutoTokenizer
import torch
from datasets import load_dataset
from scipy.special import softmax

card = "alex-miller/cva-quant-weighted-classifier"
tokenizer = AutoTokenizer.from_pretrained(card)
model = AutoModelForSequenceClassification.from_pretrained(card)


def inference(example):
    inputs = tokenizer(example['text'], return_tensors="pt")

    with torch.no_grad():
        logits = model(**inputs).logits

    predicted_class_id = logits.argmax().item()
    class_name = model.config.id2label[predicted_class_id]
    predicted_confidences = softmax(logits[0], axis=0)
    class_confidence = predicted_confidences[predicted_class_id]
    example['predicted_class'] = class_name
    example['predicted_confidence'] = class_confidence
    return example

def main():
    dataset = load_dataset("csv", data_files="CVA_project_questions.csv", split="train")
    dataset = dataset.map(inference)
    dataset.to_csv('inference_output.csv')


if __name__ == '__main__':
    main()
