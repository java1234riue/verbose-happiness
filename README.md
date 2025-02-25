---
title: "🌍 World Happiness Analysis (2015–2023)"
author: "Arnesh Tarachandani"
date: "`r Sys.Date()`"
output:
  html_document:
    theme: flatly
    toc: true
    toc_float: true
    number_sections: true
---

## 🌟 Introduction: A Data-Driven Perspective on Global Well-Being  

In today’s world, happiness and well-being are **critical indicators of societal progress**.  
The **World Happiness Report** provides valuable insights into global happiness trends, examining the socio-economic factors that shape well-being.  

This interactive **Shiny application** allows users to:  
✅ **Analyze** happiness trends from **2015–2023**.  
✅ **Explore** relationships between **GDP, social support, corruption, and more**.  
✅ **Filter dynamically** by **year, region, and key variables**.  
✅ **Visualize patterns** with an **interactive scatter plot & dynamic data table**.  

🖥 **Live App:** [Explore Here](https://shinyapps.io/your-link)  
📂 **GitHub Repo:** [View Source Code](https://github.com/your-repo-link)  
📊 **Dataset:** [World Happiness Report (2015–2023)](https://github.com/java1234riue/verbose-happiness)  

---

## 🔎 Key Insights: What the Data Reveals  

1️⃣ **Generosity vs. Corruption** – Some countries maintain **high generosity scores** despite **high corruption perceptions**, challenging assumptions.  
2️⃣ **Happiness Stability** – Some nations experience fluctuations, while others remain **remarkably stable** year after year.  
3️⃣ **Regional Differences** – Western Europe & North America lead in **life expectancy & social support**, while **lower-GDP regions** compensate with **strong community networks**.  

📊 **Dynamic Visualization:**  
```{r, echo=FALSE}
library(ggplot2)
ggplot(mtcars, aes(x=mpg, y=hp)) + 
  geom_point() + 
  labs(title="Example Scatter Plot Placeholder")
