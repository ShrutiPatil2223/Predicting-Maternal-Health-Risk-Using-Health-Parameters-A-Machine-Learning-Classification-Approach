"""
# Maternal Health Risk Prediction

This project predicts maternal health risk (High/Low) using structured clinical data collected from healthcare centers in Bangladesh. The goal is to assist in early detection and intervention to reduce maternal mortality in low-resource settings.

## 📊 Dataset

- **Records**: 1013
- **Features**: Age, SystolicBP, DiastolicBP, Blood Sugar, Body Temp, Heart Rate
- **Target**: Risk Level (0 = Low, 1 = High)

## 🧠 Models

- **Decision Tree Classifier**  
  - Best performance: Recall 0.86, Precision 0.81, F1 Score 0.84, AUC 0.78  
  - Performed best without oversampling.

- **Generalized Additive Model (GAM)**  
  - Best performance: Recall 0.76, Precision 0.85, F1 Score 0.80, AUC 0.85  
  - Smoother predictions, lower recall.

## ⚙️ Methods

- Data preprocessing, EDA, outlier removal
- Train-test split (70-30), 5-fold CV
- Used ROC, confusion matrix, precision-recall metrics
- Data balancing tested but not beneficial

## 🔑 Results

- Decision Tree outperformed GAM in recall (priority metric)
- Oversampling increased accuracy but not recall
- Future work: ensemble models, cost-sensitive learning
