#lang racket

;Gabriela Guimarães Ruggeri nº75610
;Rita Roldão de Aguiar nº75921

(require (planet aml/rosetta))

(provide (all-defined-out))

;FUNÇÕES DEFORMADORAS:

;;DIRETRIZ

;O vetor diretriz é o vetor nulo. A diretriz é uma reta vertical

(define (diretriz-prumo)
  (lambda (cota) (xyz 0 0 0)))


;A diretriz é uma reta vertical evolui ao longo de uma superfície cónica

  ;raio-inicial é o raio da base do cone a que se ajusta a hélice
  ;altura é a altura do cone
  ;omega é a velocidade angular da rotação
  ;fase representa a orietação inicial

(define (diretriz-helice-conica raio-base altura periodo fase)
  (lambda (cota)
    (define raio (* (- 1 (/ cota altura)) raio-base))
    (define angulo (* (/ 2pi periodo)(+ cota fase)))
    (xyz (* raio (cos angulo)) (* raio (sin angulo)) 0)))


;A diretriz é uma onda sinusóide que se propaga ao longo de um eixo vertical

  ;orientacao é a orientação angular relativa ao eixo Ox do referencial do bloco

(define (diretriz-sinusoidal amplitude periodo fase orientacao)
  (lambda (cota)
    (cyl (* amplitude (sin (* (/ 2pi periodo) (+ fase cota)))) orientacao 0)))


;;PERFIL

;Designamos por rácio o fator de compressão ou expansão em função da cota segundo as direções do plano horizontal.
;Se é menor do que um temos uma compressão. Se é maior do que um temos uma expansão.

;Ao longo do eixo das cotas não existe nem compressão nem expansão.

(define (perfil-vertical)
 (lambda (cota) 1))


;Ao longo do eixo das cotas e até se atingir o vértice do triângulo existe compressão (rácio menor do que um).

(define (perfil-triangular altura)
  (lambda (cota) (- 1 (/ cota altura))))


;Ao longo do eixo das cotas o rácio varia entre valores menores e maiores do que um,
;segundo a elongação de uma sinusoidal de amplitude dada.

  ;orientacao é a orientação angular relativa ao eixo Ox do referencial do bloco

(define (perfil-sinusoidal amplitude periodo fase)
  (lambda (cota)
    (+ 1 (* amplitude (sin (* (/ 2pi periodo) (+ fase cota)))))))


;Ao longo do eixo das cotas o rácio varia segundo um arco de circunferência.

(define (perfil-circular centro raio)
  (lambda (cota)
    (define diferenca-quadrados (- (sqr raio) (sqr (- cota centro))))
    (if (> diferenca-quadrados 0) (/ (sqrt diferenca-quadrados) raio) .001)))


;;TORÇÃO

;Não existe torção ao longo do eixo das cotas.

(define (torcao-nula)
  (lambda (cota) 0))


;Sempre que a cota varia de delta-cota o ângulo de torção varia de delta-angulo.

(define (torcao-delta-cota-angulo delta-cota delta-angulo) 
  (lambda (cota) (* (/ cota delta-cota) delta-angulo)))


;FUNÇÕES AUXILIARES

;Para cada ângulo cria um ponto em coordenadas polares (cota 0) com um raio dado.
;Útil para obter uma lista de vértices de um polígono regular.

(define (cria-vertices raio) (lambda (angulo) (pol raio angulo)))


;Devolve a projeção ortogonal do ponto d no plano definido pelos pontos a, b e c.
;Se o ponto d já é complanar com os restantes a projeção ortogonal é o próprio ponto.

(define (projecao-ponto a b c d)
  (let ((a1 (cx a))(a2 (cy a))(a3 (cz a))
        (b1 (cx b))(b2 (cy b))(b3 (cz b))
        (c1 (cx c))(c2 (cy c))(c3 (cz c))
        (d1 (cx d))(d2 (cy d))(d3 (cz d)))
    (let ((n1 (- (* (- b2 a2)(- c3 a3)) (* (- b3 a3)(- c2 a2))))
          (n2 (- (* (- b3 a3)(- c1 a1)) (* (- b1 a1)(- c3 a3))))
          (n3 (- (* (- b1 a1)(- c2 a2)) (* (- b2 a2)(- c1 a1)))))
      (let ((norma (+ (sqr n1) (sqr n2) (sqr n3))))
        (if (= norma 0)
            d
            (let ((k (- (/ (+ (* n1 (- d1 a1)) (* n2 (- d2 a2)) (* n3 (- d3 a3))) norma))))
                   (xyz (+ d1 (* k n1)) (+ d2 (* k n2)) (+ d3 (* k n3)))))))))


;Cria uma superfície no Rhinocerus dados quatro pontos não necessariamente complanares.
;Utiliza o procedimento projecao-ponto para obter quatro pontos complanares.

(define (superficie-projecao-r a b c d)
  (surface (polygon a b c (projecao-ponto a b c d))))


;Cria duas superfícies no Autocad dados quatro pontos não necessariamente complanares.
;Utiliza o procedimento projecao-ponto para obter quatro pontos complanares.
;Devido a restrições da função surface no Autocad o quadrilátero é dividido entre dois triângulos
; segundo uma das diagonais.

(define (superficie-projecao-a a b c d)
  (define proj-d (projecao-ponto a b c d))
  (surface (polygon b proj-d a))
  (surface (polygon b proj-d c)))


;Devolve o produto escalar dos vetores u e v.

(define (produto-escalar u v)
  (+ (* (cx u) (cx v)) (* (cy u) (cy v)) (* (cz u) (cz v))))


;Devolve o ponto médio do segmento de extremidades a e b.

(define (ponto-medio a b)
  (/c (+c a b) 2))


;Devolve o módulo de um ponto em coordenadas polares.

(define (pol-ro ponto)
  (sqrt  (+ (sqr (cx ponto)) (sqr (cy ponto)))))


;Devolve o argumento de um ponto em coordenadas polares.
;No caso do ponto (0,0,0) devolve o valor 0.
;A função atan do Racket não resolve este caso.

(define (pol-fi ponto)
  (define y (cy ponto))
  (define x (cx ponto))
  (if (and (= y 0) (= x 0))
      0
      (atan y x)))


;Cria um ponto para o desenho de uma super elipse

(define (superelipse P a b n t)
  (+xy P
       (* a (expt (sqr (cos t)) (/ 1 n)) (sgn (cos t)))
       (* b (expt (sqr (sin t)) (/ 1 n)) (sgn (sin t)))))


;Cria os pontos para o desenho de uma super elipse

(define (curva-superelipse P a b n n-pontos)
  (closed-spline 
   (map-division (lambda (t)
                   (superelipse P a b n t))
                   -pi pi n-pontos #f)))

;VARIÁVEIS GLOBAIS:

;Foram concebidas para fazer o ajustamento de blocos uns por cima de outros
; baseados na mesma secção base.
;Conseguimos desta forma, passando as referidas variáveis para o bloco seguinte,
; fazer um acoplamento perfeito sem ter de fazer mais modificações.

(define ultimo-ponto-base (xy 0 0))
(define ultima-orientacao 0)
(define ultimo-racio 1)

;FUNÇÕES TRANSFORMADORAS:

;Serve para fazer a trasnformação das coordenadas de um ponto no referencial da unidade
; para o referencial do bloco.
;Tem por argumentos as funções transformadoras e o rácio que é um parâmetro que pode ser
; passado para o bloco para produzir um efeito de compressão ou expansão segundo as direções
; horizontais para a totalidade do bloco.
;Cria uma função com a cota por argumento que gera uma outra função cujo argumento
; são as coordenadas do ponto no referencial da unidade.

(define (t-unid-bloco diretriz perfil torcao racio)
  (lambda (cota-unid)
    (lambda (ponto-unid)
      (define ro (pol-ro ponto-unid))
      (define fi (pol-fi ponto-unid))
      (define cota-bloco (+ cota-unid (cz ponto-unid)))
      (+cyl (diretriz cota-bloco)
            (* racio (perfil cota-bloco) ro)
            (+ (torcao cota-bloco) fi)
            cota-bloco))))


;Serve para fazer a trasnformação das coordenadas de um ponto no referencial do bloco
; para o referencial exterior.
;Tem por argumentos as funções as coordenadas da origem do referencial do bloco no referencial
; exterior e também o ângulo que o eixo Ox do referencial do bloco faz com o referencial exterior.
;Cria uma função cujo argumento são as coordenadas do ponto no referencial do bloco e cujo resultado
; são as coordenadas do ponto no referencial exterior.

(define (t-bloco-ext ponto-base-bloco orientacao-bloco) ;função que transforma coordenadas de bloco em coordenadas exteriores
  (lambda (ponto-bloco)
    (define ro (pol-ro ponto-bloco))
    (define fi (pol-fi ponto-bloco))
    (+c ponto-base-bloco (cyl ro (+ fi orientacao-bloco) (cz ponto-bloco)))))


;Serve para fazer a trasnformação das coordenadas de um ponto no referencial da unidade
 ;para o referencial exterior.
;Tem por argumentos a função criada por t-unid-bloco e também as coordenadas da origem do referencial
; do bloco no referencial exterior e também o ângulo que o eixo Ox do referencial do bloco faz com o referencial exterior
;Cria uma função com a cota por argumento que gera uma outra função cujo argumento
; são as coordenadas do ponto no referencial da unidade.

(define (t-unid-ext t-unid-bloco ponto-base-bloco orientacao-bloco)
  (lambda (cota-unid)
    (lambda (ponto-unid)
      ((t-bloco-ext ponto-base-bloco orientacao-bloco)
       ((t-unid-bloco cota-unid) ponto-unid)))))

;PROCEDIMENTO BLOCO:

;É o procedimento central desta solução informática cuja função é promover recursivamente a construção das sucessiva unidades
; sobrepostas passando para para o procedimento construtor da unidade, a função que permite aplicar em função da cota a
; transformação do referencial da unidade para o referencial exterior.

;Leitura dos parâmetros
;;Atribuição dos valores por defeito
(define (bloco definicoes)

  (define ponto-base (xy 0 0))
  (define executa-sempre #f)
  (define orientacao 0)
  (define const-unidade unidade-teste)
  (define altura-unidade 1)
  (define unidade-inicial 1)
  (define unidade-final 1)
  (define diretriz (diretriz-prumo))
  (define perfil (perfil-vertical))
  (define torcao (torcao-nula))
  (define racio 1)
  (define codigo 1)
  (define lista-dados '())
  (define cota-inicial -100000)
  (define cota-final 100000)
  ;Procedimento que altera os valores por defeito pelos valores entrados como argumentos.
  (define (atribui-definicoes definicoes)
    (cond ((not (null? definicoes))
           (case (car definicoes)
             ((executa-sempre) (set! executa-sempre (car (cdr definicoes))))
             ((ponto-base) (set! ponto-base (car (cdr definicoes))))
             ((orientacao) (set! orientacao (car (cdr definicoes))))
             ((unidade-inicial) (set! unidade-inicial (car (cdr definicoes))))
             ((unidade-final) (set! unidade-final (car (cdr definicoes))))
             ((const-unidade) (set! const-unidade (car (cdr definicoes))))
             ((altura-unidade) (set! altura-unidade (car (cdr definicoes))))
             ((diretriz) (set! diretriz (car (cdr definicoes))))
             ((perfil) (set! perfil (car (cdr definicoes))))
             ((torcao) (set! torcao (car (cdr definicoes))))
             ((racio) (set! racio (car (cdr definicoes))))
             ((codigo) (set! codigo (car (cdr definicoes))))
             ((lista-dados) (set! lista-dados (car (cdr definicoes))))
             ((cota-inicial) (set! cota-inicial (car (cdr definicoes))))
             ((cota-final) (set! cota-final (car (cdr definicoes))))
             (else (error "Definição desconhecida: " (car definicoes))))
           (atribui-definicoes (cdr (cdr definicoes))))))
  (atribui-definicoes definicoes)
  (define transf (t-unid-ext (t-unid-bloco diretriz perfil torcao racio)
                             ponto-base orientacao))
  (define cota-inferior (- cota-inicial (cz ponto-base)))
  (define cota-superior (- cota-final (cz ponto-base)))
  ;Procedimento recursivo que invoca o const-unidade para construir cada uma das unidades
  (define (const-unidades
           cota
           unidade)
    (cond ((> unidade unidade-final)
           ;Actualização das variáveis globais cujos valores são passados para o exterior como lista,
           ; terminada a execução
           (cond ((= codigo 0) (set! cota (* altura-unidade unidade-final))))
           (set! ultimo-ponto-base ((transf cota) (xyz 0 0 0)))
           (set! ultima-orientacao (+ (torcao cota) orientacao))
           (set! ultimo-racio (* racio (perfil cota)))
           (list 'unidade unidade
                 'ponto ultimo-ponto-base
                 'orientação ultima-orientacao
                 'racio ultimo-racio))
          (else (if (or (and (>= cota cota-inferior) (< cota cota-superior)) executa-sempre)
               (const-unidade
                cota
                unidade
                altura-unidade
                transf
                codigo
                unidade-inicial
                unidade-final
                (- cota-inicial (cz ponto-base))
                (- cota-final (cz ponto-base))
                lista-dados)
               #f)
          (const-unidades
            (+ cota altura-unidade)
            (+ unidade 1)))))
    (const-unidades
      (* (- unidade-inicial 1) altura-unidade)
      ;No caso do código ser zero a unidade inicial é igual à unidade final para terminar a execução.
      ; permitindo de qualquer forma actualizar as variáveis globais
      (if (= codigo 0) (+ unidade-final 1) unidade-inicial)))



;PROCEDIMENTO PARA TESTE DAS FUNÇÕES DEFORMADORAS:

;Permite visualizar o efeito das funções deformadoras ao longo do eixo das cotas.
;A visualização resume-se a uma linha vertical que nos permite observar a evolução da diretriz,
;e de uma linha horizontal que nos permite observar a variação do rácio e da orientação.

(define (unidade-teste cota unidade altura-unidade transf codigo u-i u-f c-i c-f lista-dados) ;procedimento que constrói a unidade/secção do edifício
  (define t (transf cota))
  (define t000 (t (xyz 0 0 0)))
  (define t00au (t (xyz 0 0 altura-unidade)))
  (define t10au (t (xyz 1 0 altura-unidade)))
  (line t000 (t (xyz 1 0 0)))
  (line t000 t00au)
  (if (= (distance t00au t10au) 0)
      #t
      (line t00au t10au)))

