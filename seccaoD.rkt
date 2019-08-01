;Gabriela Guimarães Ruggeri nº75610
;Rita Roldão de Aguiar nº75921

;;;;;;;SECÇÃO D

;Cria uma unidade do corpo da secção D.

;Criação de listas de vértices para a secção D mais externa.
(define lista-base-seccaoD (map-division (cria-vertices raio-seccaoD) pi/2 (/ (* 5 pi) 2) n-faces-seccaoD))
;Criação de listas de vértices para a secção D mais interna para quando da utiliação de volumes criar superfícis para extrudir.
(define lista-base-interior-seccaoD (map-division (cria-vertices (* .97 raio-seccaoD)) pi/2 (/ (* 5 pi) 2) n-faces-seccaoD))


;;;;;;;CONSTRUTOR DA UNIDADE DA SECÇÃO D

(define (seccaoD cota-unid unid au transf codigo u-i u-f c-i c-f lista-dados)
  ;Cria um identificador simples que constitui a função das coordenadas do ponto no referencial da unidade e as
  ; transforma nas coordenadas do referencial exterior
  (define t (transf cota-unid))
  ;Cria uma das faces do corpo da secção D
  (define (face a-base b-base ai-base bi-base)
    (define a (t a-base))
    (define b (t b-base))
    (define ai (t ai-base))
    (define bi (t bi-base))
    (define c (t (+c b-base (xyz 0 0 au))))
    (define d (t (+c a-base (xyz 0 0 au))))
    ;O tipo de desenho depende do código selecionado
    (case codigo
           ((1)
            ;Coloca a travessa horizontal na base e desenha as linhas verticais, da face em wireframe.
            (line a b) (line a d)(line b c)
            ;Para a última unidade desenha a linha de topo
            (cond ((= unid u-f)
                  (line c d)))))
    (case codigo
           ((2)
            ;Desenha a face em volume.
            (with-current-layer "Betão" (extrusion (surface (closed-line a b bi ai)) au)))))
  ;Para todas as unidades constrói na sua base uma laje com uma abertura para a caixa do elevador
  (cond ((= codigo 2)
        (with-current-layer "Betão" (subtraction
             (extrusion (surface (closed-line (map t lista-base-seccaoD))) espessura-laje-interior)
             (move (extrusion (surface (closed-line (map t lista-base-elevador))) au) (z (- (/ au 2))))))))
  ;Procedimento que constroi recursivamente todas as faces.
  (define (constroi-faces lista-pontos lista-pontosi)
    (cond
      ((not (null? (cdr lista-pontos)))
       (face (car lista-pontos) (car (cdr lista-pontos)) (car lista-pontosi) (car (cdr lista-pontosi)))
       (constroi-faces (cdr lista-pontos) (cdr lista-pontosi)))))
  ;Constrói todas as faces da secção D
  (constroi-faces lista-base-seccaoD lista-base-interior-seccaoD)
  ;Constrói 3 faces da caixa do elevador
  (constroi-faces lista-paredes-elevador lista-paredes-elevadori)
  ;Para a última unidade constrói no seu topo uma laje sem abertura para o elevador.
  (cond ((and (= unid u-f)(= codigo 2))
        (with-current-layer "Betão"
                            (extrusion
                             (surface
                              (closed-line (map (lambda (ponto) (t (+c ponto (xyz 0 0 au)))) lista-base-seccaoD))) espessura-laje-interior)))))