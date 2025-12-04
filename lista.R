d=list() #esta lista apenas seria criada na primeira vez que a lista fosse usada
#a ideia passaria por depois manter a mesma lista em usos futuros e ir atualizando
#mas caso não consigamos fazer isso podemos apenas criar uma lista sempre que iniciarmos a app

input=c(1,3,4,5,6,7,8) #o input de dados na app terá de ser convertido para este 
#formato para depois facilitar o processamento dos dados
id=as.character(input[1]) #esta conversão é essencial para que o id seja visto como
#uma key e não como um índice
valores=input[2:7] #estes são os valores que são associados a cada id e que irão ser 
#atualizados entre diferentes datas
#o que tinha pensado era adicionar as if-statements dentro do botão de guardar valores
#por exemplo, sempre que se carregava nesse botão, os dados eram verificados e, caso
#fossem válidos, eram inseridos no seu id ou criado um id novo (se o paciente não
#estivesse ainda registado) sempre que se carregasse em guardar valores
if (id %in% names(d)) { #este ciclo verifica se a id já está registada
  d[[id]]=c(d[[id]],list(valores))
} else { #caso a id seja nova
  d[[id]]=list(valores)
}

#isto foi para ver se funcionava devidamente
input=c(1,7,8,3,4,2,1)
id=as.character(input[1])
valores=input[2:7]
input=c(4,7,8,3,4,2,1)
id=as.character(input[1])
valores=input[2:7]