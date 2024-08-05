import os
from openai import OpenAI
from dotenv import load_dotenv
import tiktoken
import click
from datasets import load_dataset, concatenate_datasets, Dataset
from tqdm import tqdm
import re
from collections import Counter
import pandas as pd


load_dotenv()
client = OpenAI(
    api_key = os.getenv("OPENAI_API_KEY")
)


MODEL = "gpt-4o-mini"


def warn_user_about_tokens(tokenizer, text):
    token_cost = 0.15
    cost_per = 1000000
    token_count = len(tokenizer.encode(text))
    return click.confirm(
        "This will use at least {} tokens and cost at least ${} to run. Do you want to continue?".format(
        token_count, round((token_count / cost_per) * token_cost, 4)
    )
    , default=False)


if __name__ == '__main__':

    dataset = load_dataset("csv", data_files="CVA_flow_descriptions.csv", split="train")

    # De-duplicate
    df = pd.DataFrame(dataset)
    print(df.shape)
    df = df.drop_duplicates(subset=['text'])
    print(df.shape)
    dataset = Dataset.from_pandas(df, preserve_index=False)

    system_prompt = """
    You are tasked with classifying user text as either 'Full' or 'Partial' cash/voucher humanitarian assistance. 
    'Full' means that the text only describes cash/voucher assistance or activities funded purely by cash/voucher assistance. 
    'Partial' means that the text describes both cash/voucher assistance and other unrelated activities. 
    For e.g. text that says 'Cash, diversification of livelihoods and market interventions' would be 'Full' because all three parts are cash activities. 
    Meanwhile text that says 'Livelihood Cash and Fishery Assistance' would be 'Partial' because it describes both cash and non-cash activity. 
    'Cash for essential needs, including food, shelter and NFIs' is another example of 'Full' because all the needs are funded from the cash assistance. 
    'Cash Assistance and Access to Critical Services' is another example of 'Partial' because access to critical services is non-cash.
    Only reply 'Full' or 'Partial' in response to the user text.
    """

    tokenizer = tiktoken.encoding_for_model(MODEL)

    all_text = [system_prompt] * dataset.num_rows
    all_text += dataset['text']
    all_text = " ".join(all_text)

    if warn_user_about_tokens(tokenizer, text=all_text) == True:
        responses = list()
        for i, user_prompt in tqdm(enumerate(dataset["text"]), total=dataset.num_rows):

            messages = [
                {"role": "system", "content": system_prompt},
                {"role": "user", "content": user_prompt}
            ]
            try:
                response = client.chat.completions.create(
                    model=MODEL,
                    messages=messages
                )
                responses.append(response.choices[0].message.content)
            except:
                print("Error fetching result {} from OpenAI.".format(i))

        dataset = dataset.add_column('gpt_prediction', responses)
        dataset.to_csv('flow_inference_output_gpt.csv')
