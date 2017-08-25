## 환경변수에 (시스템변수 path)
## C:\Anaconda3\envs\tensorflow\Scripts
## C:\Anaconda3\envs 추가

devtools::install_github("rstudio/tensorflow",force=T)

Sys.setenv(TENSORFLOW_PYTHON="C:Anaconda3/envs/tensorflow")
library(tensorflow)

#tensorflow connect test
sess=tf$Session()
hello <- tf$constant('Hello,TensorFlow!')
sess$run(hello)#파이썬의 마침표를 달러로 사용.

sys<-import("sys")