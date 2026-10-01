# QUESTÃO 1

# Sorteia 40 números entre 1 e 100, permitindo repetição
bola <- sample(x = 1:100, size = 40, replace = TRUE)

# Verifica quais números são pares e conta a quantidade
pares <- (bola %% 2 == 0)
sum(pares)

# Conta quantos números sorteados são maiores que 70
sum(bola > 70)

# Identifica os números ímpares e suas respectivas posições
impares <- (bola %% 2 == 1)
posicoes <- (1:40)[impares]


# QUESTÃO 2

# Função que realiza lançamentos de um dado até obter dois resultados 4
lancamentos <- function() {
  tentativas <- 0
  contador_4 <- 0
  
  # Continua lançando enquanto não obtiver dois números 4
  while (contador_4 < 2) {
    dado <- sample(1:6, size = 1, replace = TRUE)
    tentativas <- tentativas + 1
    
    if (dado == 4) {
      contador_4 <- contador_4 + 1
    }
  }
  
  # Retorna a quantidade de lançamentos necessários
  return(tentativas)
}


# QUESTÃO 3

# Executa a função 10.000 vezes para simular o experimento
quantidades <- replicate(10000, lancamentos())

# Calcula a média de lançamentos necessários
mean(quantidades)


# QUESTÃO 4A

# Define as quatro cores e os números presentes no baralho
cores <- c("azul", "vermelha", "branca", "marrom")

baralho_cores <- c()
baralho_numeros <- c()

# Cria as 52 cartas, associando cada número a uma cor
for (cor in cores) {
  for (numero in 1:13) {
    baralho_cores <- c(baralho_cores, cor)
    baralho_numeros <- c(baralho_numeros, numero)
  }
}

# Sorteia 3 cartas diferentes do baralho
posicoes_sorteadas <- sample(x = 1:52, size = 3, replace = FALSE)

cartas_cores <- baralho_cores[posicoes_sorteadas]
cartas_numeros <- baralho_numeros[posicoes_sorteadas]

# Conta quantas das cartas sorteadas são azuis
n_azuis <- sum(cartas_cores == "azul")


# Função que sorteia 3 cartas e conta quantas são azuis
sorteia_e_conta_azuis <- function() {
  posicoes <- sample(x = 1:52, size = 3, replace = FALSE)
  cores_sorteadas <- baralho_cores[posicoes]
  
  return(sum(cores_sorteadas == "azul"))
}

# Realiza 100.000 simulações do sorteio de 3 cartas
resultados <- replicate(100000, sorteia_e_conta_azuis())


# Calcula a proporção de sorteios que possuem exatamente 2 cartas azuis
proporcao_dois_azuis <- function(n) {
  resultados <- replicate(n, sorteia_e_conta_azuis())
  proporcao <- sum(resultados == 2) / n
  
  return(proporcao)
}

proporcao_dois_azuis(100000)


# QUESTÃO 4B

tentativas <- 0
contador_7 <- 0

# Sorteia cartas com reposição até obter quatro números 7
while (contador_7 < 4) {
  posicao <- sample(x = 1:52, size = 1, replace = TRUE)
  numero_sorteado <- baralho_numeros[posicao]
  tentativas <- tentativas + 1
  
  if (numero_sorteado == 7) {
    contador_7 <- contador_7 + 1
  }
}


# QUESTÃO 5

# Função que retorna os primeiros n termos da sequência de Fibonacci
fibonacci <- function(n) {
  fib <- c(1, 1)
  
  # A partir do terceiro termo, cada valor é a soma dos dois anteriores
  for (i in 3:n) {
    fib[i] <- fib[i - 1] + fib[i - 2]
  }
  
  return(fib)
}

#_________________________________________________
# EXERCÍCIO 6
#_________________________________________________

pessoas <- c("Dwight", "Jim", "Kevin", "Creed")


# ------------------------------------------------
# EXERCÍCIO 6 (a)
# Simular uma realização do sorteio e verificar
# se o sorteio deu certo ou errado.
# ------------------------------------------------

sorteio <- sample(pessoas)

print(sorteio)

errado <- any(sorteio == pessoas)

if (errado) {
  print("O sorteio deu errado")
} else {
  print("O sorteio deu certo")
}


# ------------------------------------------------
# EXERCÍCIO 6 (b)
# Repetir o experimento 100 mil vezes e calcular
# a proporção de sorteios que deram errado.
# ------------------------------------------------

resultados <- numeric(100000)

for (i in 1:100000) {
  
  sorteio <- sample(pessoas)
  
  if (any(sorteio == pessoas)) {
    resultados[i] <- 1
  } else {
    resultados[i] <- 0
  }
}

proporcao <- mean(resultados)

print(proporcao)


# ------------------------------------------------
# EXERCÍCIO 6 (c)
# Interpretação:
# A proporção representa a frequência estimada
# de sorteios que deram errado.
# ------------------------------------------------


#_________________________________________________
# EXERCÍCIO 7
#_________________________________________________


# ------------------------------------------------
# EXERCÍCIO 7 (a)
# Simular uma partida de Craps.
# Retorna 1 se vencer e 0 se perder.
# ------------------------------------------------

jogar_craps <- function() {
  
  dado1 <- sample(1:6, 1)
  dado2 <- sample(1:6, 1)
  
  soma <- dado1 + dado2
  
  if (soma == 7 || soma == 11) {
    return(1)
  }
  
  if (soma == 2 || soma == 3 || soma == 12) {
    return(0)
  }
  
  ponto <- soma
  
  repeat {
    
    dado1 <- sample(1:6, 1)
    dado2 <- sample(1:6, 1)
    
    soma <- dado1 + dado2
    
    if (soma == ponto) {
      return(1)
    }
    
    if (soma == 7) {
      return(0)
    }
  }
}

jogar_craps()


# ------------------------------------------------
# EXERCÍCIO 7 (b)
# Repetir o experimento 100 mil vezes e calcular
# a proporção de partidas vencidas.
# ------------------------------------------------

resultados <- numeric(100000)

for (i in 1:100000) {
  resultados[i] <- jogar_craps()
}

proporcao_vitorias <- mean(resultados)

print(proporcao_vitorias)


# ------------------------------------------------
# EXERCÍCIO 7 (c)
# Interpretação:
# A proporção representa a frequência estimada
# de vitórias no jogo de Craps.
# ------------------------------------------------


#_________________________________________________
# EXERCÍCIO 8
#_________________________________________________


# ------------------------------------------------
# EXERCÍCIO 8 (a)
# Criar uma função que simula o passeio de Luke.
# Retorna 1 se ele chegar à casa e 0 se cair
# no precipício.
# ------------------------------------------------

passeio_luke <- function(L) {
  
  posicao <- L
  
  while (posicao > 0 && posicao < 20) {
    
    moeda <- sample(c("cara", "coroa"), 1)
    
    if (moeda == "cara") {
      posicao <- posicao + 1
    } else {
      posicao <- posicao - 1
    }
  }
  
  if (posicao == 20) {
    return(1)
  } else {
    return(0)
  }
}

passeio_luke(10)


# ------------------------------------------------
# EXERCÍCIO 8 (b)
# Repetir o passeio 10 mil vezes e retornar
# a proporção de vezes em que Luke chegou
# à sua casa.
# ------------------------------------------------

proporcao_luke <- function(L) {
  
  resultados <- numeric(10000)
  
  for (i in 1:10000) {
    resultados[i] <- passeio_luke(L)
  }
  
  return(mean(resultados))
}
proporcao_luke(10)

# ------------------------------------------------
# EXERCÍCIO 8 (c)
# Utilizar a função para L = 1,...,19 e
# construir o gráfico.
# ------------------------------------------------

posicoes <- 1:19
proporcoes <- numeric(19)

for (i in 1:19) {
  proporcoes[i] <- proporcao_luke(i)
}

print(proporcoes)

plot(
  posicoes,
  proporcoes,
  type = "o",
  xlab = "Posição inicial L",
  ylab = "Proporção de chegadas à casa",
  main = "Passeio aleatório de Luke"
)

#Quanto mais próximo Luke começa da posição 20, 
#maior é a probabilidade estimada de ele chegar à sua casa.

#_________________________________________________
# EXERCÍCIO 9
#_________________________________________________


# ------------------------------------------------
# EXERCÍCIO 9 (a)
# Simular um passeio de 8 passos com Link
# começando na origem e apresentar o ponto final.
# ------------------------------------------------

x <- 0
y <- 0

for (i in 1:8) {
  
  direcao <- sample(c("L", "R", "U", "D"), 1)
  
  if (direcao == "L") {
    x <- x - 1
  }
  
  if (direcao == "R") {
    x <- x + 1
  }
  
  if (direcao == "U") {
    y <- y + 1
  }
  
  if (direcao == "D") {
    y <- y - 1
  }
}

c(x, y)


# ------------------------------------------------
# EXERCÍCIO 9 (b)
# Repetir 10 mil vezes e calcular a proporção
# de vezes em que Link retornou à origem.
# ------------------------------------------------

passeio_link <- function() {
  
  x <- 0
  y <- 0
  
  for (i in 1:8) {
    
    direcao <- sample(c("L", "R", "U", "D"), 1)
    
    if (direcao == "L") {
      x <- x - 1
    }
    
    if (direcao == "R") {
      x <- x + 1
    }
    
    if (direcao == "U") {
      y <- y + 1
    }
    
    if (direcao == "D") {
      y <- y - 1
    }
  }
  
  if (x == 0 && y == 0) {
    return(1)
  } else {
    return(0)
  }
}

passeio_link()

resultados <- numeric(10000)

for (i in 1:10000) {
  resultados[i] <- passeio_link()
}

proporcao <- mean(resultados)

print(proporcao)

# A proporção representa a frequência estimada de vezes
# em que Link retorna à origem após exatamente 8 passos.
# Como o experimento é aleatório, essa proporção pode variar
# a cada execução.

# ------------------------------------------------
# EXERCÍCIO 9 (c)
# Explicação:
# É impossível retornar à origem após um número
# ímpar de passos, pois a cada passo a paridade
# da posição é alterada.
# ------------------------------------------------

# Função solicitada no item (c)
# Se N for ímpar, informa que é impossível.
# Se N for par, simula 10 mil passeios.

retorno_origem <- function(N) {
  
  if (N %% 2 == 1) {
    return("É impossível retornar à origem após um número ímpar de passos.")
  }
  
  resultados <- numeric(10000)
  
  for (i in 1:10000) {
    
    x <- 0
    y <- 0
    
    for (j in 1:N) {
      
      direcao <- sample(c("L", "R", "U", "D"), 1)
      
      if (direcao == "L") {
        x <- x - 1
      }
      
      if (direcao == "R") {
        x <- x + 1
      }
      
      if (direcao == "U") {
        y <- y + 1
      }
      
      if (direcao == "D") {
        y <- y - 1
      }
    }
    
    if (x == 0 && y == 0) {
      resultados[i] <- 1
    } else {
      resultados[i] <- 0
    }
  }
  
  return(mean(resultados))
}

# TESTE DA QUESTÃO 9(c)
retorno_origem(7)
retorno_origem(8)

#_________________________________________________
# EXERCÍCIO 10
#_________________________________________________


# ------------------------------------------------
# EXERCÍCIO 10 (a)
# Simular uma partida.
# Retorna "steven" ou "garnet".
# ------------------------------------------------

jogar <- function() {
  
  resultados <- c()
  
  repeat {
    
    moeda <- sample(c(0, 1), 1)
    
    resultados <- c(resultados, moeda)
    
    if (length(resultados) >= 3) {
      
      ultimos3 <- tail(resultados, 3)
      
      if (all(ultimos3 == c(0, 1, 0))) {
        return("steven")
      }
      
      if (all(ultimos3 == c(0, 0, 1))) {
        return("garnet")
      }
    }
  }
}

jogar()


# ------------------------------------------------
# EXERCÍCIO 10 (b)
# Repetir o experimento 10 mil vezes e calcular
# a proporção de partidas vencidas por Garnet.
# ------------------------------------------------

resultados <- character(10000)

for (i in 1:10000) {
  resultados[i] <- jogar()
}

proporcao_garnet <- mean(resultados == "garnet")

print(proporcao_garnet)


# ------------------------------------------------
# EXERCÍCIO 10 (c)
# Interpretação:
# A proporção representa a frequência estimada
# de partidas vencidas por Garnet.
# ------------------------------------------------
