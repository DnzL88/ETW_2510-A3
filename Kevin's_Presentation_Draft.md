# Presentation Draft: Intro, Literature, Cointegration, and Conclusion

*Note: This is a guide for your spoken presentation, not a rigid script. Use these points to structure your delivery and flow.*

## 1. Introduction & Literature Connection
**Goal:** Hook the audience, establish theoretical basis, and introduce the research question.

*   **The Hook:** Disposable income is the lifeblood of household financial health. Understanding what drives it in the short and long run is crucial for policymakers and economists.
*   **Literature Connection:** The professor emphasized grounding our time series model in economic theory. We drew upon several foundational concepts:
    *   **Keynesian Income Theory & Consumption:** According to Keynes (1936) and further explored by Friend (1946) in *Relationship Between Consumers' Expenditures, Savings, and Disposable Income*, there is a fundamental link where national output drives household income, which in turn drives consumption. Friend (1946) highlights the endogeneity between consumer expenditure (PCE) and disposable income (DI). 
    *   **Government Interventions:** To understand the role of policy, we look at Papadimitriou (2006) in *Government Effects on the Distribution of Income*. This literature emphasizes that government transfers act as a direct and rapid mechanism to alter income distribution and stabilize household finances during economic cycles.
    *   **Macroeconomic Aggregates:** Okun (1962) established the classic relationship between GDP and unemployment, showing how broader economic growth translates into labor market outcomes, which ultimately dictate wage earnings and disposable income.
*   **Research Question:** Building on this literature, our study asks: *How do macroeconomic aggregates (GDP, Unemployment), government policy (Transfers), and consumption (PCE) dynamically influence Disposable Income in the US from 1951 to 2025?*

## 2. Cointegration / ARDL Bounds Test
**Goal:** Explain the core methodology for finding long-run relationships clearly and confidently.

*   **Transition from Stationarity:** As my colleague previously showed, our ADF and PP tests confirmed our variables are a mixture of I(0) and I(1), with strictly no I(2) variables. This makes the ARDL framework the mathematically correct choice for our data.
*   **Model Selection:** Based on the AIC criterion, the optimal model selected was ARDL(3,3,0,3,4).
*   **The Bounds Test Results:** 
    *   We conducted the Bounds F-test to check for a long-run relationship (cointegration).
    *   Our calculated F-statistic is **6.40**.
    *   The Pesaran upper bound critical value for I(1) variables is **4.85**. 
    *   *Interpretation:* Because 6.40 is far above 4.85, we firmly reject the null hypothesis. We have robust statistical proof of cointegration. This means the relationship between GDP, transfers, unemployment, PCE, and disposable income isn't just a short-term spurious correlation—they move together toward a long-term equilibrium.
*   **Error Correction Term (ECT):** 
    *   Our ECT is **-0.157** and is highly significant (p < 0.0001). 
    *   *Why this matters:* The negative sign is crucial—it mathematically guarantees that the system converges back to equilibrium after an economic shock. Specifically, **15.7%** of any deviation is corrected each quarter. This implies it takes roughly 6.4 quarters (about 1.5 years) for the economy to fully absorb a shock to disposable income.

## 3. Conclusion & Implications
**Goal:** Summarize the findings, address limitations transparently, and end with real-world policy implications.

*   **Summary of Key Findings:**
    *   **Long-Run:** GDP is the undisputed, dominant long-run driver. A 1% increase in GDP yields a 0.68% increase in Disposable Income, reflecting the gradual, structural transmission of economic growth into wages.
    *   **Short-Run:** Federal Government Transfers are the short-run hero. They show an immediate, statistically significant positive impact (+0.16% in the same quarter), proving the efficiency of government disbursements as an economic stabilizer.
    *   **Testing the Feedback Loop (PCE):** We included Consumption (PCE) as a regressor to test for macroeconomic feedback (i.e., does consumption drive aggregate demand, which then raises incomes?). We found PCE to be statistically insignificant. This is actually an excellent result for our model's integrity. It confirms that Disposable Income is driven 'top-down' by structural GDP growth and government policy, rather than 'bottom-up' by consumer spending. This allows us to rule out severe reverse-causality (simultaneity bias) issues.
*   **Model Reliability:** We want to be fully transparent about our diagnostics. While Breusch-Pagan and RESET tests passed smoothly, we did detect higher-order serial correlation. To ensure our statistical inferences (t-stats and p-values) remained completely valid, we applied robust HAC (Newey-West) standard errors. Furthermore, CUSUM and MOSUM tests confirmed our model is structurally stable over the 70+ year sample period.
*   **Final Policy Implication:** If policymakers want to structurally elevate household wealth over the long term, they must focus on broad GDP growth. However, when households need immediate relief (like during a recession or pandemic), direct government transfers are the most rapid and effective policy lever.
