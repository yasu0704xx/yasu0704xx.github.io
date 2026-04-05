# install.packages(c("np", "ggplot2"))

library(np)
library(ggplot2)

set.seed(21574) # as you like it

#--------------------------------------------------
# 設定
#--------------------------------------------------
n_list <- c(500, 1000, 5000, 10000)
x_grid <- seq(-4, 4, length.out = 1000)

# Epanechnikov kernel + N(0,1) のときの理論的 MSE-optimal bandwidth
# h_opt = (40 * sqrt(pi) / n)^(1/5)
h_opt <- function(n) {
  (40 * sqrt(pi) / n)^(1/5)
}

# np パッケージで KDE を計算する関数
fit_np_kde <- function(x, x_grid, h) {
  bw_obj <- npudensbw(
    dat = x,
    bws = h,
    bandwidth.compute = FALSE,
    bwtype = "fixed",
    ckertype = "epanechnikov",
    ckerorder = 2
  )
  
  fhat <- npudens(
    bws = bw_obj,
    tdat = x,
    edat = x_grid
  )
  
  as.numeric(fitted(fhat))
}

#--------------------------------------------------
# シミュレーション（各 n について1回）
#--------------------------------------------------
kde_df_list <- list()
rug_df_list <- list()
bw_table <- data.frame()

for (n in n_list) {
  
  # 標本生成
  x <- rnorm(n, mean = 0, sd = 1)
  
  # MSE-optimal bandwidth
  h0 <- h_opt(n)
  
  # 3種類の bandwidth
  h_set <- c(
    "MSE-opt の半分" = 0.5 * h0,
    "MSE-opt"       = h0,
    "MSE-opt の2倍"  = 2.0 * h0
  )
  
  # bandwidth の記録
  bw_table <- rbind(
    bw_table,
    data.frame(
      n = n,
      バンド幅 = names(h_set),
      h = as.numeric(h_set)
    )
  )
  
  # 真の密度
  true_df_n <- data.frame(
    x = x_grid,
    density = dnorm(x_grid),
    type = "真の密度",
    n = factor(n, levels = n_list)
  )
  
  kde_df_list[[paste0("true_", n)]] <- true_df_n
  
  # KDE
  for (bw_name in names(h_set)) {
    h <- h_set[bw_name]
    fhat <- fit_np_kde(x, x_grid, h)
    
    kde_df_list[[paste0("kde_", n, "_", bw_name)]] <- data.frame(
      x = x_grid,
      density = fhat,
      type = bw_name,
      n = factor(n, levels = n_list)
    )
  }
  
  # rug plot 用
  rug_df_list[[paste0("rug_", n)]] <- data.frame(
    x = x,
    n = factor(n, levels = n_list)
  )
}

plot_df <- do.call(rbind, kde_df_list)
rug_df  <- do.call(rbind, rug_df_list)

# 線種・色の順序を固定
plot_df$type <- factor(
  plot_df$type,
  levels = c("真の密度", "MSE-opt の半分", "MSE-opt", "MSE-opt の2倍")
)

#--------------------------------------------------
# ggplot2 で描画
#--------------------------------------------------
p <- ggplot(plot_df, aes(x = x, y = density, color = type, linetype = type)) +
  geom_line(linewidth = 0.9) +
  facet_wrap(
    ~ n,
    ncol = 2,
    scales = "fixed",
    labeller = labeller(n = function(x) paste0("n=", x))
  ) +
  labs(
    title = "カーネル密度推定量",
    subtitle = "データ生成過程：標準正規分布，カーネル：Epanechnikov",
    x = "x",
    y = "密度"
  ) +
  scale_color_manual(
    values = c(
      "真の密度" = "black",
      "MSE-opt の半分" = "red",
      "MSE-opt" = "blue",
      "MSE-opt の2倍" = "darkgreen"
    )
  ) +
  guides(
    color = guide_legend(title = NULL),
    linetype = guide_legend(title = NULL)
  ) +
  theme_bw(base_size = 13) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold")
  )

print(p)

ggsave(
  filename = "../00-Latex/input/01-kde.png",
  plot = p,
  width = 12,
  height = 8,
  dpi = 300
)

# 使用した bandwidth を表示
print(bw_table)
