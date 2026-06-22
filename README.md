# Modelagem Espacial do Esgotamento Sanitário em São Paulo via Regressão Beta

Este repositório contém os scripts em R, bases de dados e o relatório estatístico de um estudo focado na modelação dos determinantes sociodemográficos e de infraestrutura que explicam a proporção de esgotamento sanitário nos 645 municípios do Estado de São Paulo.

### 🎯 Objetivo e Cenário de Simulação
O acesso ao saneamento básico é um dos indicadores mais críticos de desenvolvimento urbano e saúde pública. Este projeto utiliza modelos lineares generalizados, especificamente a Regressão Beta, para analisar como fatores como o abastecimento de água, a recolha de lixo, a densidade demográfica e a educação impactam a cobertura da rede de esgotos no território paulista.

Devido à natureza da variável resposta — uma proporção estritamente delimitada no intervalo contínuo $(0, 1)$ e com forte assimetria —, a Regressão Beta demonstrou ser a abordagem metodológica mais rigorosa e adequada.

### 🛠️ Metodologia e Estrutura Teórica
* **Dados:** IBGE / SIDRA (Tabelas 6803, 9858, 4714, 6805,6892) do Censo de 2022
* **Variável Resposta:**  proporção de domicílios particulares permanentes ligados à rede geral de esgotamento sanitário, rede pluvial ou fossa ligada à rede
* **Variáveis Preditvas:** Taxa de Alfabetização (X1), Densidade Demográfica (X2), Proporção de Água de Rede Geral (X3), Proporção de Coleta de Lixo (X4), Localização Metropolitana (X5)
* **Modelo:** Regressão Beta com função de ligação logit.

---

### 📊 Análise de Resultados e Evidências Visuais

#### 1. **Ajuste Global:** 
O modelo apresentou uma excelente qualidade de ajustamento, com um Pseudo-$R^2$ de Cox-Snell de 0,7031, sendo capaz de explicar cerca de 70% da variabilidade da infraestrutura de esgotos no Estado.

#### 2. **Preditor Principal:** 
A infraestrutura de abastecimento de água emergiu como o preditor positivo mais forte, evidenciando o acoplamento da expansão das redes subterrâneas. A tabela abaixo com as Razões de Chances (OR) e Intervalos de Confiança (IC 95%) indica que um aumento de um desvio padrão na variável água está associado com uma multiplicação de aproximadamente 2,27 vezes na proporção esperada de cobertura de esgoto, **evidenciando que as obras de saneamento subterrâneo
e de distribuição hídrica expandem-se de forma conjunta.** 

<img width="427" height="316" alt="image" src="https://github.com/user-attachments/assets/cbe43afe-68ec-485a-bf3a-52ef9e6611f6" />

#### 3. **Heterogeneidade e Influência:** 
Uma rigorosa análise de diagnóstico (Distância de Cook, Influência Local e Alavancagem) identificou 104 municípios atípicos. A análise de sensibilidade comprovou que 97,4% da Região Metropolitana de São Paulo (RMSP) atua como um polo de alavancagem, refletindo a profunda desigualdade territorial e o "Paradoxo Metropolitano" (superadensamento vs. vastas áreas rurais). A sumarização dessas observações influentes está explícita na tabela abaixo.
  
<img width="726" height="145" alt="image" src="https://github.com/user-attachments/assets/da9ccaac-5991-4131-87d0-59a0aeb50885" />

---

### 📂 Estrutura do Repositório
O projeto segue as melhores práticas de portabilidade e organização de código:
* `scripts/`: Código em R com a coleta de dados, análise exploratória e modelagem com diagnóstico.
* `dados/`: Base de dados em .csv.
* `relatorio/`: Relatório técnico em PDF e Latex contendo o desenvolvimento matemático das funções.
* `*.Rproj`: Ficheiro de projeto do RStudio para gestão de caminhos relativos.

### ⚙️ Como Executar
1. Descarregue ou clone este repositório.
2. Abra o ficheiro `.Rproj` no RStudio.
3. Execute o script contido na pasta `scripts/`.
