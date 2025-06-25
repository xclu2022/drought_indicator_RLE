# library(parallel)
# library(foreach)
# library(doParallel)
# 
# # 并行批量下载
# parallel_download <- function(urls, folder = "D:/BEES/RLE/Data/", n_cores = 4) {
#   # 设置并行环境
#   cl <- makeCluster(n_cores)
#   registerDoParallel(cl)
#   
#   if (!dir.exists(folder)) {
#     dir.create(folder, recursive = TRUE)
#   }
#   
#   results <- foreach(i = 1:length(urls), .packages = c("httr")) %dopar% {
#     url <- urls[i]
#     filename <- basename(url)
#     filepath <- file.path(folder, filename)
#     
#     tryCatch({
#       download.file(url, filepath, mode = "wb")
#       return(paste("成功:", filename))
#     }, error = function(e) {
#       return(paste("失败:", url, "-", e$message))
#     })
#   }
#   
#   stopCluster(cl)
#   return(results)
# }
# # urls="https://thredds.nci.org.au/thredds/catalog/rq0/level_2/71/COLUMNMAXREFLECTIVITY/catalog.html"
# parallel_download(urls)
# 
# 
# library(rvest)
# library(dplyr)
# 
# # 爬取网页上的所有下载链接
# scrape_and_download <- function(base_url, file_extensions = c("nc")) { # file_extensions = c("csv", "pdf", "xlsx")
#   # 读取网页
#   page <- read_html(base_url)
#   
#   # 获取所有链接
#   links <- page %>% 
#     html_nodes("a") %>% 
#     html_attr("href")
#   
#   # 过滤文件链接
#   file_links <- links[grepl(paste(file_extensions, collapse = "|"), links, ignore.case = TRUE)]
#   
#   # 转换为绝对URL
#   absolute_links <- ifelse(
#     grepl("^http", file_links),
#     file_links,
#     paste0(base_url, "/", file_links)
#   )
#   
#   # 批量下载
#   batch_download(absolute_links)
#   
#   return(absolute_links)
# }
# 
# # 使用示例
# downloaded_files <- scrape_and_download(urls)


# 批量下载文件
library(httr)
library(rvest)

# 基础批量下载函数
batch_download <- function(urls, folder = "downloads") {
  # 创建下载文件夹
  if (!dir.exists(folder)) {
    dir.create(folder, recursive = TRUE)
  }
  
  for (i in seq_along(urls)) {
    url <- urls[i]
    filename <- basename(url)
    
    # 如果没有文件名，生成一个
    if (filename == "" || !grepl("\\.", filename)) {
      filename <- paste0("file_", i, ".data")
    }
    
    filepath <- file.path(folder, filename)
    
    tryCatch({
      download.file(url, filepath, mode = "wb")
      cat("已下载:", filename, "\n")
      Sys.sleep(1)  # 添加延迟
    }, error = function(e) {
      cat("下载失败:", url, "-", e$message, "\n")
    })
  }
}

# 使用示例
urls <- paste0("https://thredds.nci.org.au/thredds/fileServer/rq0/level_2/71/COLUMNMAXREFLECTIVITY/71_", 20141201:20141231, "_columnmaxreflectivity.nc")
urls
# c(
#   "https://example.com/data1.csv",
#   "https://example.com/data2.pdf",
#   "https://example.com/data3.xlsx"
# )

batch_download(urls)


library(raster)
library(ncdf4)

columnmaxreflectivity <- raster("C:/Users/2007l/Documents/downloads/71_20141231_columnmaxreflectivity.nc") # "Z:/ck/xc/Greening/pre_1982_1984/pre_1982_1984.nc"
plot(columnmaxreflectivity)
points(sites$Easting, sites$Northing)

# Step 1: Convert dBZ to instantaneous rain rate (mm/h)
dBZ_to_rain_rate_mm_per_hr <- function(ref) {
  #' Convert reflectivity (dBZ) to instantaneous rain rate (mm/h)
  #' using a Marshall–Palmer-like relation.
  #' 
  #' @param ref Reflectivity in dBZ (numeric vector or single value)
  #' @return Rain rate in mm/h (numeric vector or single value)
  #' 
  #' @examples
  #' # Single value
  #' dBZ_to_rain_rate_mm_per_hr(30)
  #' 
  #' # Vector of values
  #' reflectivity_values <- c(20, 25, 30, 35, 40)
  #' rain_rates <- dBZ_to_rain_rate_mm_per_hr(reflectivity_values)
  #' print(rain_rates)
  
  # Convert dBZ to linear reflectivity factor Z
  Z <- 10^(ref / 10.0)
  
  # Apply Marshall-Palmer relationship: Z = aR^b
  # Rearranged to: R = (Z/a)^(1/b)
  # Where a = 240.3 and b = 1.51 (typical values)
  R_mm_per_hr <- (Z / 240.3)^(1 / 1.51)
  
  return(R_mm_per_hr)
}

# 使用示例和测试
cat("=== 雷达反射率转降雨率转换函数 ===\n")

# 测试单个值
single_dbz <- 30
single_rain_rate <- dBZ_to_rain_rate_mm_per_hr(single_dbz)
cat("dBZ =", single_dbz, "-> Rain rate =", round(single_rain_rate, 2), "mm/h\n")

# 测试多个值
dbz_values <- c(10, 20, 25, 30, 35, 40, 45, 50)
rain_rates <- dBZ_to_rain_rate_mm_per_hr(dbz_values)

# 创建结果数据框
results <- data.frame(
  dBZ = dbz_values,
  Rain_Rate_mm_h = round(rain_rates, 3)
)

print(results)

# 可视化转换关系
if (require(ggplot2, quietly = TRUE)) {
  # 创建更详细的数据用于绘图
  dbz_range <- seq(0, 60, by = 0.5)
  rain_range <- dBZ_to_rain_rate_mm_per_hr(dbz_range)
  
  plot_data <- data.frame(
    dBZ = dbz_range,
    Rain_Rate = rain_range
  )
  
  # 绘制转换曲线
  p <- ggplot(plot_data, aes(x = dBZ, y = Rain_Rate)) +
    geom_line(color = "blue", size = 1) +
    labs(
      title = "雷达反射率 vs 降雨率转换关系",
      subtitle = "基于 Marshall-Palmer 关系",
      x = "反射率 (dBZ)",
      y = "降雨率 (mm/h)"
    ) +
    theme_minimal() +
    scale_y_log10() +
    annotation_logticks(sides = "l")
  
  print(p)
} else {
  # 如果没有 ggplot2，使用基础绘图
  dbz_range <- seq(0, 60, by = 1)
  rain_range <- dBZ_to_rain_rate_mm_per_hr(dbz_range)
  
  plot(dbz_range, rain_range, 
       type = "l", 
       col = "blue", 
       lwd = 2,
       xlab = "反射率 (dBZ)", 
       ylab = "降雨率 (mm/h)",
       main = "雷达反射率 vs 降雨率转换关系",
       log = "y")
  grid()
}

# 额外的实用函数：批量处理数据框
process_radar_data <- function(data, dbz_column = "reflectivity") {
  #' 批量处理雷达数据框，添加降雨率列
  #' 
  #' @param data 包含反射率数据的数据框
  #' @param dbz_column 反射率列的名称
  #' @return 添加了降雨率列的数据框
  
  if (!dbz_column %in% names(data)) {
    stop(paste("列", dbz_column, "在数据框中不存在"))
  }
  
  data$rain_rate_mm_h <- dBZ_to_rain_rate_mm_per_hr(data[[dbz_column]])
  return(data)
}

cat("\n=== 函数转换完成 ===\n")
cat("主要函数: dBZ_to_rain_rate_mm_per_hr(ref)\n")
cat("辅助函数: process_radar_data(data, dbz_column)\n")


# 单个值转换
rain_rate <- dBZ_to_rain_rate_mm_per_hr(30)

# 向量转换
dbz_vector <- c(20, 25, 30, 35)
rain_rates <- dBZ_to_rain_rate_mm_per_hr(dbz_vector)

# 数据框批量处理
df <- data.frame(reflectivity = c(25, 30, 35))
df_with_rain <- process_radar_data(df, "reflectivity")
df_with_rain

# 加载必要的包
library(ncdf4)      # 读取netCDF文件
library(ggplot2)    # 绘图
library(reshape2)   # 数据重塑
library(viridis)    # 颜色映射
library(fields)     # 图像绘制

# 设置索引 (R使用1-based索引)
idx <- 15  # 对应Python中的14 (1,3, 7, 14)

# 假设你已经有了数据，这里展示如何从netCDF文件读取
nc_file <- nc_open("C:/Users/2007l/Documents/downloads/71_20141231_columnmaxreflectivity.nc")
max_rain_rates <- ncvar_get(nc_file, "columnmaxreflectivity")
longitude <- ncvar_get(nc_file, "longitude")
latitude <- ncvar_get(nc_file, "latitude")
nc_close(nc_file)

# 提取指定时间索引的数据
data <- max_rain_rates[,,2]  # 3D数组的第idx个时间切片
!all(is.na(data))

# 获取经纬度坐标
lon2d <- longitude  # 形状 (y, x)
lat2d <- latitude   # 形状 (y, x)

# 转换为1D数组
if(length(dim(lon2d)) == 2) {
  # 如果坐标是2D的，提取1D数组
  lon_1d <- lon2d[1, ]  # 第一行
  lat_1d <- lat2d[, 1]  # 第一列
} else {
  # 如果坐标已经是1D的
  lon_1d <- lon2d
  lat_1d <- lat2d
}

# 方法1：使用base R的image()函数
# 设置图形参数
par(mfrow=c(1,1), mar=c(4,4,3,2))

# 创建图像
image(lon_1d, lat_1d, t(data), 
      col = viridis(100),
      xlab = "经度 (Longitude)", 
      ylab = "纬度 (Latitude)",
      main = paste("最大降雨率 - 时间索引", idx))

# 添加等高线
contour(lon_1d, lat_1d, t(data), add = TRUE, col = "white", alpha = 0.5)

# 添加颜色条
image.plot(lon_1d, lat_1d, t(data), 
           col = viridis(100),
           xlab = "经度 (Longitude)", 
           ylab = "纬度 (Latitude)",
           main = paste("最大降雨率 - 时间索引", idx),
           legend.lab = "降雨率 (mm/h)")

# 方法2：使用ggplot2 (更现代的方法)
# 将数据转换为长格式
data_df <- expand.grid(lon = lon_1d, lat = lat_1d)
data_df$rain_rate <- as.vector(t(data))

# 创建ggplot图
p <- ggplot(data_df, aes(x = lon, y = lat, fill = rain_rate)) +
  geom_raster() +
  scale_fill_viridis_c(name = "降雨率\n(mm/h)") +
  labs(x = "经度 (Longitude)",
       y = "纬度 (Latitude)",
       title = paste("最大降雨率 - 时间索引", idx)) +
  theme_minimal() +
  theme(aspect.ratio = 1,  # 保持地理数据的等比例
        panel.grid = element_line(alpha = 0.3)) +
  coord_fixed()  # 固定坐标比例

print(p)

# 方法3：使用lattice包的levelplot
library(lattice)

# 创建网格数据
grid_data <- expand.grid(lon = lon_1d, lat = lat_1d)
grid_data$rain_rate <- as.vector(t(data))

# 创建levelplot
levelplot(rain_rate ~ lon * lat, data = grid_data,
          col.regions = viridis(100),
          xlab = "经度 (Longitude)",
          ylab = "纬度 (Latitude)",
          main = paste("最大降雨率 - 时间索引", idx),
          aspect = "iso",  # 等比例
          contour = TRUE,
          pretty = TRUE)

# 方法4：如果你有sf包，可以处理空间数据
# library(sf)
# library(stars)
# 
# # 将数据转换为stars对象
# rain_stars <- st_as_stars(list(rain_rate = data), 
#                          dimensions = st_dimensions(x = lon_1d, y = lat_1d))
# 
# # 绘图
# plot(rain_stars, col = viridis(100), 
#      main = paste("最大降雨率 - 时间索引", idx))

# 保存图像
# ggsave("rainfall_map.png", plot = p, width = 8, height = 6, dpi = 300)
# 或者使用base R
# png("rainfall_map.png", width = 800, height = 600, res = 100)
# image.plot(lon_1d, lat_1d, t(data), col = viridis(100))
# dev.off()