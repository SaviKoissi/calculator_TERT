import torch
import pandas as pd
from torch.utils.data import DataLoader, TensorDataset

# Load the PyTorch model
model = torch.load("/Users/koissi/Desktop/Oncology/TERT/Model_TERT/TBD/trained_model.pth")

# Define a function for model inference
def predict(ref, position, mut):
    # Create input data for the model as a data frame
    input_data = pd.DataFrame({
        'Ref': [ref],
        'Position': [position],
        'Mut': [mut]
    })
    
    # Convert input data to a PyTorch tensor
    input_tensor = torch.tensor(input_data.values, dtype=torch.float32)
    
    # Make predictions using the model
    predictions = model(input_tensor)
    
    return predictions.item()
