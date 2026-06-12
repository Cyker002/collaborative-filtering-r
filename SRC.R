

#funções

#Input dos nomes dos usuários
obter_usuarios <- function() {
  entrada <- readline(prompt = "Digite os nomes dos usuários separados por vírgula: ")
  nomes <- trimws(unlist(strsplit(entrada, ",")))
  return(nomes)
}

#gerar matriz de usuários dinâmicos por filmes predefindos(10)
gerar_matriz_teste <- function(nomes_usuarios, catalogo_filmes) {
  qtd_u <- length(nomes_usuarios)
  qtd_f <- length(catalogo_filmes)
  
  # cria matriz vazia
  matriz <- matrix(NA, nrow = qtd_u, ncol = qtd_f)
  rownames(matriz) <- nomes_usuarios
  colnames(matriz) <- catalogo_filmes
  
  # Preenche com notas aleatórias e NAs 
  matriz[] <- sample(
    c(1, 2, 3, 4, 5, NA), 
    size = (qtd_u * qtd_f), 
    replace = TRUE, 
    prob = c(0.05, 0.05, 0.05, 0.1, 0.05, 0.70)
  )
  
  return(matriz)
}

#multiplica a matriz pela sua transposta para descobrir a afinidade 
calcular_similaridade <- function(matriz_notas) {
  matriz_calc <- matriz_notas
  matriz_calc[is.na(matriz_calc)] <- 0 # Tratamento de dados vazios
   
  # matriz * Transposta
  similaridade <- matriz_calc %*% t(matriz_calc)
  
  diag(similaridade) <- 0 # Zera a diagonal (ninguém é recomendado para si mesmo)
  return(similaridade)
}


gerar_recomendacao <- function(usuario_alvo, matriz_notas, matriz_similaridade) {
  
  # isola as afinidades do alvo
  afinidades <- matriz_similaridade[usuario_alvo, ]
  
  # descobre o amigo
  melhor_amigo <- names(sort(afinidades, decreasing = TRUE))[1]
  
  # se a maior afinidade for 0, o alvo não tem nada em comum com ninguém
  if(afinidades[melhor_amigo] == 0) {
    return("Não há usuários com gostos similares suficientes para gerar uma recomendação.")
  }
  
  # descobrir qual a maior nota do amigo 
  notas_do_amigo <- matriz_notas[melhor_amigo, ]
  
  # achar os filmes com nota Max
  notas_validas_amigo <- notas_do_amigo[!is.na(notas_do_amigo)] 
  maior_nota_encontrada <- max(notas_validas_amigo)
  
  # filtra os filmes com a mesma nota
  filmes_que_o_amigo_amou <- names(notas_do_amigo[!is.na(notas_do_amigo) & notas_do_amigo == maior_nota_encontrada])
  
  # 4. pega a linha do alvo e descobre o que ele ainda não viu
  notas_do_alvo <- matriz_notas[usuario_alvo, ]
  filmes_nao_vistos <- names(notas_do_alvo[is.na(notas_do_alvo)])
  
  # filmes com a nota máxima do amigo E que o alvo não viu
  sugestoes_finais <- intersect(filmes_que_o_amigo_amou, filmes_nao_vistos)
  
  if(length(sugestoes_finais) == 0) {
    sugestoes_finais <- "Nenhuma recomendação nova no momento."
  }
  
  # Retorna o resultado e mostra qual foi a nota base usada
  resultado <- list(
    Vizinho_Mais_Proximo = melhor_amigo,
    Nota_Base_Da_Recomendacao = maior_nota_encontrada,
    Filmes_Recomendados = sugestoes_finais
  )
  
  return(resultado)
}


main <- function() {
  # Catálogo
  catalogo_padrao <- c("Matrix", "Duna", "Avatar", "Inception", "Interstellar", 
                       "Gladiador", "Alien", "Shrek", "Titanic", "Coringa",
                       "Rocky", "Tubarão", "Halloween", "Psicose", "O Iluminado",
                       "Toy Story", "Up", "Vingadores", "Batman", "Se7en")
  
  print("--- INICIALIZANDO O SISTEMA ---")
  meus_usuarios <- obter_usuarios()
  
  # Verificação de segurança inicial
  if(length(meus_usuarios) > 0 && meus_usuarios[1] != "") {
    
    matriz_principal <- gerar_matriz_teste(meus_usuarios, catalogo_padrao)
    matriz_sim <- calcular_similaridade(matriz_principal)
    
    print("Notas_Usuários")
    print(matriz_principal)
    
    print("Afinidade_Usuários")
    print(matriz_sim)
  
  
    while(TRUE) {
      cat("\n--------------------------------------------------\n")
      usuario_alvo <- readline(prompt = "Digite o nome do usuário para recomendação (ou 'sair' para encerrar): ")
      usuario_alvo <- trimws(usuario_alvo) # Limpa os espaços invisíveis
      
      if(tolower(usuario_alvo) == "sair") {
        print("Encerrando o sistema.")
        break 
      }
      
      
      # verifica se o nome digitado existe na lista original
      if(usuario_alvo %in% meus_usuarios) {
        
        print(paste("--- Recomendações para o usuário:", usuario_alvo, "---"))
        resultado <- gerar_recomendacao(usuario_alvo, matriz_principal, matriz_sim)
        print(resultado)
        
      } else {
        print(paste("Erro: O usuário '", usuario_alvo, "' não existe no sistema. Tente novamente.", sep=""))
      }
    }
    
  } else {
    print("Erro: Nenhum usuário foi fornecido para inicializar o sistema.")
  }
}


main()
