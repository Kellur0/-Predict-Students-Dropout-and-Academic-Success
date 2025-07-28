# 🎓 Student Dropout & Academic Success Prediction

This project focuses on predicting student dropout and academic success using a comprehensive dataset from the UCI Machine Learning Repository. The dataset includes demographic, socio-economic, and academic performance information of students across programs like Agronomy, Nursing, Journalism, and Management. Conducted in collaboration with a diverse team, this project provides insights into key risk factors and predictive models to aid student retention strategies.

## 📊 Exploratory Data Analysis Highlights

- Identified significant predictors such as **age**, **admission grades**, and **parental education**
- Discovered dropout trends influenced by **course selection** and **admission criteria**
- Noted **class imbalance** in dropout vs. non-dropout rates, highlighting the need for specialized preprocessing

## 🧹 Data Cleaning & Preprocessing

- Handled missing values and assessed distribution of continuous features
- Merged rare categorical levels to reduce noise
- Generated **dummy variables** for categorical features
- Created a **correlation matrix** to identify potential multicollinearity

## 🧠 Model Implementation

| Model                    | Cutoff | Sensitivity | Specificity |
|--------------------------|--------|-------------|-------------|
| Logistic Regression      | 0.1    | 93.4%       | 59.8%       |
| K-Nearest Neighbors (KNN)| 0.1    | 94.5%       | 49.6%       |
| Classification Tree      | 0.1    | 100%        | 0%          |

> ⚖️ Logistic Regression provided the most balanced performance, offering a strong trade-off between identifying at-risk students and minimizing false positives.

## 🧾 Conclusion

Logistic Regression emerged as the most effective model, achieving **93.4% sensitivity** and **59.8% specificity**, enabling early identification of at-risk students while maintaining reliable precision. This model offers a data-driven foundation for designing student support systems to enhance retention and success.

## 💡 Key Takeaway

Predictive analytics can play a pivotal role in solving complex educational challenges. This project demonstrates the power of data in informing **targeted interventions** that support student achievement and reduce dropout rates.

