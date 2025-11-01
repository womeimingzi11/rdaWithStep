# 安装所需的R包
cat("开始安装rdaWithStep项目的依赖包...\n")

# 设置CRAN镜像
repos_url <- 'https://cran.r-project.org'

# 主要依赖包列表
main_packages <- c('shiny', 'shinythemes', 'DT', 'tidyverse', 'vegan')

# 尝试安装主要包
cat("\n安装主要依赖包...\n")
install_result <- tryCatch({
  install.packages(main_packages, repos = repos_url, dependencies = TRUE, INSTALL_opts = '--no-lock')
  TRUE
}, error = function(e) {
  cat("警告：安装过程中出现错误，但会继续尝试安装可用的包。\n")
  FALSE
})

# 检查已安装的包
installed_packages <- installed.packages()
installed_names <- rownames(installed_packages)

# 报告主要包的安装状态
cat("\n主要包安装状态：\n")
for (pkg in main_packages) {
  if (pkg %in% installed_names) {
    cat(sprintf("✓ %s 已成功安装\n", pkg))
  } else {
    cat(sprintf("✗ %s 安装失败\n", pkg))
  }
}

# 尝试安装ggvegan
cat("\n尝试安装ggvegan包...\n")
ggvegan_installed <- FALSE

# 方法1：尝试从CRAN安装
cat("方法1：尝试从CRAN安装...\n")
tryCatch({
  install.packages('ggvegan', repos = repos_url)
  if ('ggvegan' %in% installed.packages()[,1]) {
    cat("✓ ggvegan 从CRAN成功安装\n")
    ggvegan_installed <- TRUE
  }
}, error = function(e) {
  cat("✗ 从CRAN安装ggvegan失败\n")
})

# 如果方法1失败，尝试从GitHub安装
if (!ggvegan_installed) {
  cat("\n方法2：尝试从GitHub安装remotes包...\n")
  tryCatch({
    install.packages('remotes', repos = repos_url)
    cat("✓ remotes 包安装成功\n")
    
    cat("方法2：尝试从GitHub安装ggvegan...\n")
    remotes::install_github('gavinsimpson/ggvegan')
    if ('ggvegan' %in% installed.packages()[,1]) {
      cat("✓ ggvegan 从GitHub成功安装\n")
      ggvegan_installed <- TRUE
    }
  }, error = function(e) {
    cat("✗ 从GitHub安装ggvegan失败\n")
  })
}

# 最终总结
cat("\n========= 安装总结 =========\n")
cat("项目：rdaWithStep\n")
cat("主要依赖包：shiny, shinythemes, DT, tidyverse, vegan\n")
cat("可选依赖包：ggvegan\n\n")

# 检查是否所有必需的包都已安装
required_packages <- main_packages
all_required_installed <- all(required_packages %in% installed_names)

if (all_required_installed) {
  cat("✅ 所有必需的依赖包已成功安装！\n")
} else {
  cat("❌ 部分必需的依赖包安装失败。请查看上面的错误信息。\n")
}

if (ggvegan_installed) {
  cat("✅ ggvegan 包已成功安装。\n")
} else {
  cat("⚠️ ggvegan 包未安装。某些功能可能无法正常工作。\n")
  cat("   建议：您可以尝试手动从GitHub安装: devtools::install_github('gavinsimpson/ggvegan')\n")
}

cat("\n安装完成！现在您可以尝试运行应用程序了。\n")
cat("运行方法: 在R或RStudio中执行 runApp() 或点击'Run App'按钮。\n")