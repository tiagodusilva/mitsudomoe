# Mitsudomoe
Projeto desenvolvido no âmbito da unidade curricular PLOG.

![Prolog Initial Board](docs/images/prolog_initial_board.png)

## Identificação

### Grupo Mitsudomoe_5
* Tiago Duarte da Silva - up201806516 - Turma 3
* Ricardo Jorge Cruz Fontão - up201806317 - Turma 3

## Instruções

Instalar e correr o SICStus prolog, caso esteja a ser usado o terminal dedicado em windows, aconselhamos a ir a `Settings > Font` e trocar para uma *font monospaced* (por exemplo Consolas).

Para correr o jogo em si, corra `(re)consult('game.pl')` e corra a função `play` sem argumentos.

## Regras de jogo

[Livro de Regras - Mitsudomoe](docs/MITSUDOMOE_EN.pdf)

### Objetivo

Chegar com as 3 bolas ao canto do jogador adversário.

### Equipamento

* Tabuleiro de 5x5
* 3 bolas para cada jogador
* 8 anéis para cada jogador

### Preparação

Cada jogador começa por colocar 3 anéis no seu canto e 1 bola em cima de cada anel conforme a imagem.

![Initial board](docs/images/initial_board.png)

### Regras Gerais
* Quando um componente entra em jogo, não pode voltar a sair.
* Uma bola pode apenas ser colocada em cima de um anel da mesma cor.
* Adjacente pode ser ortogonalmente ou diagonalmente.
* Quando uma bola chega a um dos espaços finais não pode voltar a sair.

### Turno

Um turno consiste em dois passos:

1. Colocar no tabuleiro ou mexer um dos próprios anéis
2. Mexer uma das próprias bolas

Se não for possível uma jogada, o jogador perde automaticamente.
No caso de existirem 3 bolas **de qualquer cor** nos home spaces de um jogador no final do seu turno, ele perderá o jogo.

#### Mover um anel

Nesta fase podemos colocar um anel de fora do tabuleiro ou recolocar um que esteja já em jogo. Só podem ser movidos anéis que estejam expostos (não tenham qualquer bola ou anel por cima). O lugar onde se coloca o anel jogado pode ser qualquer um que não tenha uma bola em cima. Se tiver anéis nessa casa coloca-se por cima deles.

#### Mexer uma bola

A primeira possibilidade é mexer a nossa bola para uma casa adjacente que tenha um anel nosso exposto em cima.

A segunda opção é "passar" por cima, em linha reta, de quantas bolas adjacentes houver, tendo que aterrar num anel exposto do próprio jogador. De seguida o jogar tem de recolocar qualquer bola que passou por cima que pertença a um adversário para uma posição válida(anel da mesma cor). Se alguma das bolas não poder ser recolocada o salto não pode ser feito.

![Ball vault](docs/images/ball_vault.png)

### Final de jogo

Se no fim do turno do jogador todas as suas bolas estiverem no canto oposto do início do jogo, esse jogador ganha.


## Representação interna do estado de jogo

A representação interna do jogo (GameState) consiste de uma lista de 4 elementos:

1. O primeiro elemento é uma lista de listas de *stacks* (listas) em que cada stack representa uma célula do jogo e armazena as peças lá colocadas preservando a sua ordem (ordem crescente de índice corresponde a ordem crescente de altura).
2. O segundo elemento é o número de anéis brancos que não estão em jogo.
3. O terceiro elemento é o número de anéis pretos que não estão em jogo.
4. O quarto elemento representa a quantidade de peças que são representadas de cada célula, de forma a que o jogo fique "legível".

Relativamente a cada peça usamos uma representação númerica diferente, por exemplo, um anel branco pode ser representado pelo número inteiro 1 e um anel preto pelo número inteiro 2. 

No caso de uma célula estar vazia a lista que a representa também é vazia.

Representações do estado inicial e de um possível estado intermédio e final:
```prolog
% 1 : White Ring
% 2 : Black Ring
% 3 : White Ball
% 4 : Black Ball

initial(GameState) :-
    GameState = [
        [  % Game board
            [ [],     [],     [],  [2, 4], [2, 4]],
            [ [],     [],     [],  [],     [2, 4]],
            [ [],     [],     [],  [],     []],
            [ [1, 3], [],     [],  [],     []],
            [ [1, 3], [1, 3], [],  [],     []]
        ],
        5, % Unplayed white rings
        5, % Unplayed black rings
        3  % Shown Stack Size
    ].

mid_game(GameState) :-
    GameState = [
        [  % Game board
            [ [],     [],     [],        [2, 1], [1, 3]],
            [ [],     [1, 3], [],        [2, 4], [2, 4]],
            [ [],     [1],    [2, 1, 3], [2],    [1]],
            [ [2, 4], [2, 1], [2],       [],     []],
            [ [],     [],     [],        [],     []]
        ],
        1, % Unplayed white rings
        0, % Unplayed black rings
        4  % Shown Stack Size
    ].

end_game(GameState) :-
    GameState = [
        [  % Game board
            [ [],     [],     [],     [1, 2, 1, 3], [1, 3]],
            [ [],     [],     [],     [2, 4],       [2, 1, 3]],
            [ [],     [1],    [2, 1], [],           []],
            [ [2, 4], [2, 1], [2, 1], [],           []],
            [ [2, 4], [],     [],     [],           []]
        ],
        0, % Unplayed white rings
        0, % Unplayed black rings
        5  % Shown Stack Size
    ].
```

A informação relativa a que jogador joga a seguir é guardada em **Player**:
* 0 -> Branco
* 1 -> Preto


## Visualização do estado de jogo

O predicado [`display_game\2`](display.pl) desenha no ecrã um tabuleiro de qualquer dimensão até 27x27. Para cada linha do tabuleiro é chamado o predicado `print_line\4`, que é divido em top, mid e bot, desenha as linhas necessárias na consola. Cada célula pode ser identificada por um par número e letra, inspirado pelo sistema do xadrez.


No canto inferior esquerdo de cada célula é ainda mostrado o número total de peças na *stack* respetiva. Este número pode ser alterado ao longo do jogo e define ainda o tamanho de  cada célula (em altura e largura) para o tabuleiro ser sempre de visualização fácil. A largura de cada célula é sempre um número ímpar para garantir que o espaçamento entre os vários elementos é correto.


O predicado `print_line_mid\4` é responsável por desenhar as N peças no topo da *stack*, mas sempre encostadas à base da *frame* da célula. Para este efeito, o predicado `get_stuffed_elem_from_end0\s` dá-nos qual o código da peça numa dada posição da *stack*, mas como que se estivesse *stuffed* (por exemplo: [1, 2] seria interpretado como [1, 2, 0] para a visualização permanecer correta, mas sem nunca alterar a lista).


## Bibliografia

- [Livro de Regras - Mitsudomoe](docs/MITSUDOMOE_EN.pdf)
- [Documentação ofical do SICStus](https://sicstus.sics.se/documentation.html)


## Screenshots

### Estado de jogo inicial
![Initial Game State](docs/images/prolog_initial_board.png)

### Estado de jogo intermédio
![Initial Game State](docs/images/prolog_finished_state.png)

### Estado de jogo final
![Initial Game State](docs/images/prolog_intermediate_state.png)


## Lógica de jogo

### Representação do estado de jogo

Pode se encontrado [aqui](#representação-interna-do-estado-de-jogo).

### Visualização de estado do jogo

A visualização do jogo é feita da mesma forma do relatório intercalar como pode ser visto [aqui](#visualização-do-estado-de-jogo), com a exceção de as células estarem um poucos mais esticadas horizontalmente, depois do feedback do docente.

Relativamente aos menus introduzidos para a seleção de jogadas, dos níveis da AI e para a seleção do modo de jogo, estes podem ser encontrados no ficheiro [input.pl](input.pl).

Para a leitura do modo de jogo usamos o predicado [read_mode/1](input.pl), que mostra a mensagem com as opções possíveis e depois lê a opção do utilizador.

![Exemplo Modos](docs/images/modes.png)

Para a leitura da dificuldade da AI, se for necessário em função do modo de jogo, lemos a dificuldade (random ou smart) para a AI se o modo for *Humano vs AI* ou duas vezes se o modo for *AI vs AI*.

![Exemplo Niveis](docs/images/levels.png)

Para a leitura de cada jogada e de acordo com a estrutura de cada jogada, começamos por perguntar ao utilizador se quer por um anel novo em campo ou  mover um que ja esteja em campo, sempre perguntando as coordenadas. De seguida as coordenadas da bola que quer mover e as coordenadas de destino.

Toda a introdução de coordenadas pode ser feita na forma de "A3" ou "3A", ou seja, coluna e linha ou vice-versa, com as letras maiúsculas ou minúsculas. Os inputs também não necessitam de um ponto final no fim.

![Exemplo Jogada](docs/images/coords.png)

### Estruturação de um *move*

De forma a representar cada **move** nós usamos uma lista de listas:

As coordenadas são sempre representadas em [Row, Col] e um displace é [Coordenadas Iniciais, Coordenadas Finais].

**Move -> [DisplaceRing, DisplaceBall, [Displaces], Player]**

**DisplaceRing** -> Displace do anel a mexer (Coordenadas iniciais são [-1,-1] se a jogada for colocar um anel novo em jogo)

**DisplaceBall** -> Displace da bola a mexer

**[Displaces]** -> Displaces às bolas inimigas no caso de ocorrer um vault (vazia se não houver ou não ocorrer um vault)

**Player** -> Jogador que realiza a jogada (white/black)

### Lista de jogadas válidas


### Execução de jogadas

O nosso predicado **move/3** está divido em 3 partes


## Conclusões


## Bibliografia