# Author:  Yi-Ying Chen 
# Contact: yiyingchen@as.edu.tw
# date: 2026-03-04


# 載入套件
library(FactoMineR)
library(factoextra)


# import the data set 

df <- read.csv("PCA_PFT24_2000.csv", header = TRUE, sep = ",")
# 快速檢查資料結構
head(df)
str(df)
# 1. 為了方便閱讀，重新命名欄位（可選）
colnames(df) <- c("Lat", "Lon", "Rainf", "Temp", "Humidity", 
                  "Evap", "LW_net", "SW_net", "Veget_max", "GPP")

# 2. 篩選用於 PCA 的數值型變數
# 我們排除經緯度 (Lat, Lon)，因為它們是地理位置而非氣象機制
pca_data <- df[, c("Rainf", "Temp", "Humidity", "Evap", 
                   "LW_net", "SW_net")]

# 3. 檢查是否有 NA 值（PCA 不允許缺失值）
pca_data <- na.omit(pca_data)


# 執行 PCA，scale.unit = TRUE 代表進行標準化
res.pca <- PCA(pca_data, scale.unit = TRUE, graph = FALSE)

# 查看各主成分的貢獻率 (特徵值)
print(get_eig(res.pca))

# 提取變數在各個 PC 上的 Loading
loadings <- res.pca$var$coord

# 轉換成資料框並整理
4loadings_df <- as.data.frame(loadings)
print(loadings_df)


# 繪製 PC1 的變數貢獻度
p1 <- fviz_contrib(res.pca, choice = "var", axes = 1, fill = "skyblue", color = "black") +
  labs(title = "Contributions to Dim 1 (PC1)")

# 繪製 PC2 的變數貢獻度
p2 <- fviz_contrib(res.pca, choice = "var", axes = 2, fill = "salmon", color = "black") +
  labs(title = "Contributions to Dim 2 (PC2)")

# 合併顯示
library(gridExtra)
grid.arrange(p1, p2, ncol = 2)



# 繪製變數 PCA 圖，並加大標題與座標軸字體
plot_par_pc <- fviz_pca_var(res.pca, 
             col.var = "contrib", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE) + 
  labs(title = "Variables PCA - Meteorological Factors",
       x = "PC1", y = "PC2") +
  theme_minimal() + # 使用簡潔主題作為基礎
  theme(
    # 總標題：大小 20, 加粗
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    
    # 座標軸標題 (PC1, PC2)：大小 18, 加粗
    axis.title = element_text(size = 18, face = "bold"),
    
    # 座標軸刻度數字：大小 16, 加粗, 純黑色
    axis.text = element_text(size = 16, face = "bold", color = "black"),
    
    # 圖例標題與文字：大小 14, 加粗
    legend.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 12, face = "bold")
  )

plot(plot_par_pc)

dev.new()


#
# 1. 提取前兩個主成分的分數 (PC Scores)
pc_scores <- as.data.frame(res.pca$ind$coord[, 1:2])
colnames(pc_scores) <- c("PC1", "PC2")

# 2. 將原始的 GPP 數據與這些 PC 分數合併
regression_data <- cbind(GPP = df$GPP, pc_scores)

# 檢查前幾行
head(regression_data)

# 3. 建立線性回歸模型
model <- lm(GPP ~ PC1 + PC2, data = regression_data)

# 4. 查看回歸統計摘要
summary(model)
# 5. 繪製 觀測值 vs 預測值
regression_data$Predicted <- predict(model)

library(ggplot2)

# 假設你已經有了 regression_data 並計算出 Predicted
# 這裡我們將圖表物件存入 p
p <- ggplot(regression_data, aes(x = Predicted, y = GPP)) +
  geom_point(alpha = 0.6, color = "#2c3e50", size = 2) + # 稍微加大點的大小
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red", size = 1.2) +
  labs(
    title = "GPP Prediction: Observed vs Predicted (via PCs)",
    subtitle = paste("R-squared:", round(summary(model)$r.squared, 3)),
    x = "Predicted GPP (from PC1 & PC2)",
    y = "Observed GPP (Original Data)"
  ) +
  theme_bw() + # 使用簡潔底色
  theme(
    # 總標題字體：大小 18, 加粗, 置中
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    # 副標題字體：大小 14
    plot.subtitle = element_text(size = 14, hjust = 0.5),
    # 座標軸標題 (X, Y)：大小 16, 加粗
    axis.title = element_text(size = 16, face = "bold"),
    # 座標軸刻度數字：大小 14, 加粗, 黑色
    axis.text = element_text(size = 14, face = "bold", color = "black"),
    # 圖例字體（如果有）：大小 12
    legend.text = element_text(size = 12, face = "bold"),
    # 增加邊距讓文字不擁擠
    plot.margin = margin(10, 10, 10, 10)
  )
# 顯示圖表
print(p)


