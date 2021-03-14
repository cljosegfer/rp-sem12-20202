rm(list=ls())

# svm
library(kernlab)

# view
MostraImagem = function(x, main = 'imagem', n = 64){
  rotate = function(x) t(apply(x, 2, rev))
  img = matrix(x, nrow = n)
  cor = rev(grey(50:1/50))
  image(rotate(img), col = cor, main = main)
}

# algoritmo
slice = function(center, length){
  l = (length - 1) / 2
  return(seq(center - l, center + l))
}

convolution = function(X, filter, stride = 1, op = 'normal'){
  image.shape = dim(X)
  filter.shape = dim(filter)
  
  l = floor((filter.shape[1] - 1) / 2)
  out = matrix(data = 0, nrow = image.shape[1] - filter.shape[1] - 1, 
               ncol = image.shape[2] - filter.shape[2] - 1)
  
  i = 1
  y = 1
  while(y < image.shape[2] - stride){
    y = y + stride
    slicey = slice(center = y, length = filter.shape[2])
    x = 1
    while(x < image.shape[1] - stride){
      x = x + stride
      slicex = slice(center = x, length = filter.shape[1])
      window = X[slicex, slicey]
      out[i] = sum(window * filter) / length(filter)
      i = i + 1
    }
  }
  
  return(out)
}

ReLU = function(X){
  X[X<0] = 0
  return(X)
}

pooling = function(X, filter.shape = c(2, 2), stride = 2, op = 'max'){
  image.shape = dim(X)
  xlim = ceiling(image.shape[1] / stride)
  ylim = ceiling(image.shape[2] / stride)
  out = matrix(data = 0, nrow = xlim, ncol = ylim)
  
  i = 1
  y = 1
  while(y <= image.shape[2]){
    x = 1
    while(x <= image.shape[1]){
      if(x == image.shape[1] && y != image.shape[2]){
        window = X[x, y:(y + filter.shape[2] - 1)]
      }
      else if(x != image.shape[1] && y == image.shape[2]){
        window = X[x:(x + filter.shape[1] - 1), y]
      }
      else if(x == image.shape[1] && y == image.shape[2]){
        window = X[x, y]
      }
      else {
        window = X[x:(x + filter.shape[1] - 1), y:(y + filter.shape[2] - 1)]
      }
      if(op == 'max') out[i] = max(window)
      i = i + 1
      x = x + stride
    }
    y = y + stride
  }
  
  return(out)
}

cnn = function(x, type){
  X = matrix(data = x, ncol = 9)
  filter = filtro(type = type)
  c = convolution(X = X, filter = filter, op = 'normalized')
  C = matrix(data = ReLU(X = c), ncol = 7)
  S = pooling(X = C)
}

# dataset
sintetico = function(){
  cinza = 1
  preto = -1
  dataset = matrix(data = 0, ncol = 81, nrow = 11)
  
  X = c(rep(cinza, 9), 
        cinza, preto, rep(cinza, 5), preto, cinza, 
        rep(cinza, 2), preto, rep(cinza, 3), preto, rep(cinza, 2), 
        rep(cinza, 3), preto, cinza, preto, rep(cinza, 3), 
        rep(cinza, 4), preto, rep(cinza, 4), 
        rep(cinza, 3), preto, cinza, preto, rep(cinza, 3), 
        rep(cinza, 2), preto, rep(cinza, 3), preto, rep(cinza, 2), 
        cinza, preto, rep(cinza, 5), preto, cinza, 
        rep(cinza, 9))
  dataset[1, ] = X
  
  X = c(rep(cinza, 9), 
        rep(cinza, 2), preto, rep(cinza, 5), preto, 
        rep(cinza, 3), preto, rep(cinza, 3), preto, cinza, 
        rep(cinza, 4), preto, cinza, preto, rep(cinza, 2), 
        rep(cinza, 5), preto, rep(cinza, 3), 
        rep(cinza, 4), preto, cinza, preto, rep(cinza, 2), 
        rep(cinza, 3), preto, rep(cinza, 3), preto, cinza, 
        rep(cinza, 2), preto, rep(cinza, 5), preto, 
        rep(cinza, 9))
  dataset[2, ] = X
  
  X = c(rep(cinza, 9), 
        rep(cinza, 9), 
        cinza, preto, rep(cinza, 5), preto, cinza, 
        rep(cinza, 2), preto, rep(cinza, 3), preto, rep(cinza, 2), 
        rep(cinza, 3), preto, cinza, preto, rep(cinza, 3), 
        rep(cinza, 4), preto, rep(cinza, 4), 
        rep(cinza, 3), preto, cinza, preto, rep(cinza, 3), 
        rep(cinza, 2), preto, rep(cinza, 3), preto, rep(cinza, 2), 
        cinza, preto, rep(cinza, 5), preto, cinza)
  dataset[3, ] = X
  
  X = c(rep(cinza, 9), 
        rep(cinza, 9), 
        rep(cinza, 2), preto, rep(cinza, 5), preto, 
        rep(cinza, 3), preto, rep(cinza, 3), preto, cinza, 
        rep(cinza, 4), preto, cinza, preto, rep(cinza, 2), 
        rep(cinza, 5), preto, rep(cinza, 3), 
        rep(cinza, 4), preto, cinza, preto, rep(cinza, 2), 
        rep(cinza, 3), preto, rep(cinza, 3), preto, cinza, 
        rep(cinza, 2), preto, rep(cinza, 5), preto)
  dataset[4, ] = X
  
  X = c(rep(cinza, 9), 
        preto, rep(cinza, 5), preto, rep(cinza, 2), 
        cinza, preto, rep(cinza, 3), preto, rep(cinza, 3), 
        rep(cinza, 2), preto, cinza, preto, rep(cinza, 4), 
        rep(cinza, 3), preto, rep(cinza, 5), 
        rep(cinza, 2), preto, cinza, preto, rep(cinza, 4), 
        cinza, preto, rep(cinza, 3), preto, rep(cinza, 3), 
        preto, rep(cinza, 5), preto, rep(cinza, 2), 
        rep(cinza, 9))
  dataset[5, ] = X
  
  X = c(rep(cinza, 9), 
        rep(cinza, 9), 
        rep(cinza, 2), rep(preto, 5), rep(cinza, 2), 
        rep(cinza, 2), preto, rep(cinza, 3), preto, rep(cinza, 2), 
        rep(cinza, 2), preto, rep(cinza, 3), preto, rep(cinza, 2), 
        rep(cinza, 2), preto, rep(cinza, 3), preto, rep(cinza, 2), 
        rep(cinza, 2), preto, rep(cinza, 3), preto, rep(cinza, 2), 
        rep(cinza, 9), 
        rep(cinza, 9))
  dataset[6, ] = X
  
  X = c(rep(cinza, 9), 
        rep(cinza, 9), 
        rep(cinza, 3), rep(preto, 5), cinza, 
        rep(cinza, 3), preto, rep(cinza, 3), preto, cinza, 
        rep(cinza, 3), preto, rep(cinza, 3), preto, cinza, 
        rep(cinza, 3), preto, rep(cinza, 3), preto, cinza, 
        rep(cinza, 3), preto, rep(cinza, 3), preto, cinza, 
        rep(cinza, 9), 
        rep(cinza, 9))
  dataset[7, ] = X
  
  X = c(rep(cinza, 9), 
        rep(cinza, 9), 
        cinza, rep(preto, 5), rep(cinza, 3), 
        cinza, preto, rep(cinza, 3), preto, rep(cinza, 3), 
        cinza, preto, rep(cinza, 3), preto, rep(cinza, 3), 
        cinza, preto, rep(cinza, 3), preto, rep(cinza, 3), 
        cinza, preto, rep(cinza, 3), preto, rep(cinza, 3), 
        rep(cinza, 9), 
        rep(cinza, 9))
  dataset[8, ] = X
  
  X = c(rep(cinza, 9), 
        rep(cinza, 9), 
        rep(cinza, 9), 
        cinza, rep(preto, 5), rep(cinza, 3), 
        cinza, preto, rep(cinza, 3), preto, rep(cinza, 3), 
        cinza, preto, rep(cinza, 3), preto, rep(cinza, 3), 
        cinza, preto, rep(cinza, 3), preto, rep(cinza, 3), 
        cinza, preto, rep(cinza, 3), preto, rep(cinza, 3), 
        rep(cinza, 9))
  dataset[9, ] = X
  
  X = c(rep(cinza, 9), 
        rep(cinza, 9), 
        rep(cinza, 9), 
        rep(cinza, 9), 
        cinza, rep(preto, 5), rep(cinza, 3), 
        cinza, preto, rep(cinza, 3), preto, rep(cinza, 3), 
        cinza, preto, rep(cinza, 3), preto, rep(cinza, 3), 
        cinza, preto, rep(cinza, 3), preto, rep(cinza, 3), 
        cinza, preto, rep(cinza, 3), preto, rep(cinza, 3))
  dataset[10, ] = X
  
  X = c(rep(cinza, 9), 
        rep(cinza, 2), preto, rep(cinza, 3), preto, rep(cinza, 2), 
        rep(cinza, 2), rep(preto, 2), cinza, preto, rep(cinza, 3), 
        rep(cinza, 3), rep(preto, 2), rep(cinza, 4), 
        rep(cinza, 4), rep(preto, 2), rep(cinza, 3), 
        rep(cinza, 3), preto, cinza, rep(preto, 2), rep(cinza, 2), 
        rep(cinza, 2), preto, rep(cinza, 3), preto, rep(cinza, 2), 
        rep(cinza, 9), 
        rep(cinza, 9))
  dataset[11, ] = X
  
  return(dataset)
}

# filtros
filtro = function(type){
  f = matrix(0, ncol = 3, nrow = 3)
  if(type == 1) f = matrix(c(1, -1, -1, 
                             -1, 1, -1, 
                             -1, -1, 1), ncol = 3)
  if(type == 2) f = matrix(c(1, -1, 1, 
                             -1, 1, -1, 
                             1, -1, 1), ncol = 3)
  if(type == 3) f = matrix(c(1, 1, 1, 
                             1, -1, -1, 
                             1, -1, -1), ncol = 3)
  if(type == 4) f = matrix(c(1, 1, 1, 
                             -1, -1, 1, 
                             -1, -1, 1), ncol = 3)
  return(f)
}

# main
data.un = sintetico()

# filtros
filtro1 = filtro(1)
filtro2 = filtro(2)
filtro3 = filtro(3)
filtro4 = filtro(4)

# cnn
data = matrix(data = 0, nrow = nrow(data.un), ncol = 16)
type = 4
for(amostra in seq(nrow(data.un))){
  data[amostra, ] = cnn(data.un[amostra, ], type = type)
}

# # view
# for(i in 1:11){
#   image = matrix(data = data.un[i, ], ncol = 9)
#   MostraImagem(image, n = 9, main = paste('Imagem', i))
#   image = matrix(data = data[i, ], ncol = 4)
#   MostraImagem(image, n = 4, main = paste('CNN, filtro', type, '- Imagem', i))
# }

# rotulos de train
Y = c(rep(1, 5), rep(-1, 5))

# train e test
train = data.un[-nrow(data), ]
test = matrix(data = data.un[nrow(data), ], nrow = 1)

# param svm
svm.type = 'C-bsvc'
kernel = 'rbfdot'
kpar = 'automatic'
C = 1

# metodo
erro = 0
for(epoca in seq(100)){
  # shuffle
  shuffle = sample(nrow(train))
  train = train[shuffle, ]
  Y = Y[shuffle]
  
  # svm
  model = ksvm(x = train, y = Y, 
               type = svm.type, kernel = kernel, kpar = kpar, C = C, scale = FALSE)
  yhat = predict(object = model, newdata = test)
  
  # avaliacao
  if(yhat != 1) erro = erro + 1
}

mean = 1 - erro / 100
sd = sd(c(rep(1, erro), rep(0, 100 - erro)))
print(paste('tipo:', type, 'acuracia:', mean, sd))
