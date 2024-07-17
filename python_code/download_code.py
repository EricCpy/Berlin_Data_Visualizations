import os
import requests

url = '' #TODO download link
save_dir = './data'
os.makedirs(save_dir, exist_ok=True)
file_path = os.path.join(save_dir, 'listings.csv')
response = requests.get(url)
if response.status_code == 200:
    with open(file_path, 'wb') as file:
        file.write(response.content)
    print(f'File successfully downloaded and saved to {file_path}')
else:
    print(f'Failed to download file. Status code: {response.status_code}')