# =======================================================
# PROJETO: SISTEMA DE RECOMENDAÇÃO MATRICIAL (MODULAR)
# =======================================================

# -------------------------------------------------------
# 1. ÁREA DOS SUBPROGRAMAS (FUNÇÕES)
# -------------------------------------------------------

# Função 1: Captura e limpa os nomes digitados no Console
obter_usuarios <- function() {
  entrada <- readline(prompt = "Digite os nomes dos usuários separados por vírgula: ")
  nomes <- trimws(unlist(strsplit(entrada, ",")))
  return(nomes)
}

# Função 2: Constrói a matriz com o catálogo estático e os usuários dinâmicos
gerar_matriz_teste <- function(nomes_usuarios, catalogo_filmes) {
  qtd_u <- length(nomes_usuarios)
  qtd_f <- length(catalogo_filmes)
  
  # Cria matriz vazia e adiciona as etiquetas
  matriz <- matrix(NA, nrow = qtd_u, ncol = qtd_f)
  rownames(matriz) <- nomes_usuarios
  colnames(matriz) <- catalogo_filmes
  
  # Preenche com notas aleatórias e NAs para simular o banco de dados
  matriz[] <- sample(
    c(1, 2, 3, 4, 5, NA), 
    size = (qtd_u * qtd_f), 
    replace = TRUE, 
    prob = c(0.1, 0.1, 0.2, 0.2, 0.1, 0.3)
  )
  
  return(matriz)
}

# Função 3: O Motor Matemático (Álgebra Linear)
calcular_similaridade <- function(matriz_notas) {
  matriz_calc <- matriz_notas
  matriz_calc[is.na(matriz_calc)] <- 0 # Tratamento de dados vazios
  
  # Processo matricial: Matriz * Transposta
  similaridade <- matriz_calc %*% t(matriz_calc)
  
  diag(similaridade) <- 0 # Zera a diagonal (ninguém é recomendado para si mesmo)
  return(similaridade)
}

# Função 4: O Algoritmo de Sugestão
gerar_recomendacao <- function(usuario_alvo, matriz_notas, matriz_similaridade) {
  matriz_calc <- matriz_notas
  matriz_calc[is.na(matriz_calc)] <- 0 
  
  # Descobre quais filmes o usuário alvo ainda não viu
  filmes_nao_vistos <- is.na(matriz_notas[usuario_alvo, ])
  
  # Proteção: Se ele já viu tudo, encerra o subprograma
  if(!any(filmes_nao_vistos)) {
    return("Este usuário já avaliou todos os filmes do catálogo.")
  }
  
  # Multiplica a linha de afinidade pelas notas do banco de dados (drop=FALSE protege a dimensão)
  notas_estimadas <- matriz_similaridade[usuario_alvo, , drop = FALSE] %*% matriz_calc
  
  # Filtra apenas o que não foi visto e ordena do melhor para o pior
  recomendacoes <- notas_estimadas[1, filmes_nao_vistos]
  recomendacoes_ordenadas <- sort(recomendacoes, decreasing = TRUE)
  
  return(recomendacoes_ordenadas)
}


# -------------------------------------------------------
# 2. ÁREA DE EXECUÇÃO (O SEU "MAIN")
# -------------------------------------------------------
# Encapsulando tudo em uma função principal para evitar bugs no RStudio

main <- function() {
  # Catálogo fixo (Até 10 filmes)
  catalogo_padrao <- c("Matrix", "Duna", "Avatar", "Inception", "Interstellar", 
                       "Gladiador", "Alien", "Shrek", "Titanic", "Coringa")
  
  print("--- INICIALIZANDO O SISTEMA ---")
  meus_usuarios <- obter_usuarios()
  
  # Verificação de segurança
  if(length(meus_usuarios) > 0 && meus_usuarios[1] != "") {
    
    matriz_principal <- gerar_matriz_teste(meus_usuarios, catalogo_padrao)
    print("--- Matriz de Notas (Banco de Dados) ---")
    print(matriz_principal)
    
    matriz_sim <- calcular_similaridade(matriz_principal)
    print("--- Matriz de Similaridade ---")
    print(matriz_sim)
    
    usuario_teste <- meus_usuarios[1] 
    print(paste("--- Recomendações para o usuário:", usuario_teste, "---"))
    
    resultado <- gerar_recomendacao(usuario_teste, matriz_principal, matriz_sim)
    print(resultado)
    
  } else {
    print("Erro: Nenhum usuário foi fornecido para inicializar o sistema.")
  }
}

# -------------------------------------------------------
# INICIAR PROGRAMA
# -------------------------------------------------------
# Esta é a única linha que realmente "roda" solta no script
main()