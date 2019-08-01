;Gabriela Guimarães Ruggeri nº75610
;Rita Roldão de Aguiar nº75921

;;;;;;;BASE DA COBERTURA

;Cria uma unidade da base da cobertura. Toda a base é criada numa única unidade.

(define (base-cobertura cota-unid unid au transf codigo u-i u-f c-i c-f lista-dados)
  ;Cria um identificador simples que constitui a função das coordenadas do ponto no referencial da unidade e as
  ; transforma nas coordenadas do referencial exterior
  (define t (transf cota-unid))
  ;Cria uma das faces da base da cobertura
  (define (seccao-base a-base b-base)
    ;Obtenção das coordenadas dos pontos no referencial exterior.
    (define a (t a-base))
    (define b (t b-base))
    (define c (t (+c b-base (xyz 0 0 au))))
    (define d (t (+c a-base (xyz 0 0 au))))
    (define e (/c (+c c d) 2))
    (define ae (-c e a))
    (define be (-c e b))
    ;Cria uma secção horizontal da face da base da cobertura.
    (define (itera-seccao unid pae-ant pad-ant pbe-ant pbc-ant)
      (cond
        ((<= unid nut-base)
         (define pad (t (+c a-base (xyz 0 0 (* unid aut)))))
         (define pbc (t (+c b-base (xyz 0 0 (* unid aut)))))
         (define pae (+c a (*c ae (/ unid nut-base))))
         (define pbe (+c b (*c be (/ unid nut-base))))
         ;O tipo de desenho depende do código selecionado.
         (case codigo
           ((1)
            ;Desenha as travessas horizontais da secção da base da cobertura em wireframe
            (line pad pae) (line pbc pbe)))
         (case codigo
           ((2 3)
            ;Desenha as travessas horizontais da secção da base da cobertura com volumes
            (with-current-layer "Aço"
                (cylinder pad espessura-travessas-cobertura pae)
                (cylinder pbc espessura-travessas-cobertura pbe))))
         ;No caso de se ter escolhido o código para o efeito (3) coloca os vidros na base da secção.
         ;A grande quantidade de condições que surgem deve-se a dificuldades do Autocad para lidar com superfícies.
         (case codigo
           ((3)
            (cond
                 ((= unid 1)
                  (case backend-usado
                   ((0)
                    (with-current-layer "Vidro"
                      (extrusion (superficie-projecao-r pae-ant pae pad pad-ant) espessura-vidros-cobertura)
                      (extrusion (surface (polygon b pbe pbc)) espessura-vidros-cobertura)
                      ))
                   ((1)
                    (with-current-layer "Vidro"
                      (thicken (superficie-projecao-r pae-ant pae pad pad-ant)
                          espessura-vidros-cobertura)
                      (thicken (superficie-projecao-r pbe-ant pbe pbc pbc-ant)
                          espessura-vidros-cobertura)))))
                 (else
                  (case backend-usado
                   ((0)
                    (with-current-layer "Vidro"
                      (extrusion (superficie-projecao-a pae-ant pae pad pad-ant) espessura-vidros-cobertura)
                      (extrusion (superficie-projecao-a pbe-ant pbe pbc pbc-ant) espessura-vidros-cobertura)
                      ))
                   ((1)
                    (with-current-layer "Vidro"
                      (thicken (superficie-projecao-r pae-ant pae pad pad-ant)
                          espessura-vidros-cobertura)
                      (thicken (superficie-projecao-r pbe-ant pbe pbc pbc-ant)
                          espessura-vidros-cobertura))))))))
         ;Coloca as travessas paralelas nas janelas do lado esquerdo da secção
         (coloca-travessas pad-ant pae-ant pae (projecao-ponto pad-ant pae-ant pae pad)
                              distancia-travessas-vidros
                              espessura-travessas-vidros
                              codigo)
         ;Coloca as travessas paralelas nas janelas do lado direito da secção
         (coloca-travessas pbe-ant pbc-ant pbc (projecao-ponto pbe-ant pbc-ant pbc pbe)
                              distancia-travessas-vidros
                              espessura-travessas-vidros
                              codigo)
         ;Procedimento que constroi recursivamente todas as secções.
         (itera-seccao (+ unid 1) pae pad pbe pbc))))
    (itera-seccao 1 a a b b)
    (case codigo
           ((1)
            ;Cria os lados do triângulo em wireframe.
            (line a e) (line b e)))
    (case codigo
           ((2 3)
            ;Cria os lados do triângulo em volume.
            (with-current-layer "Aço"
                (cylinder a espessura-diagonais-base-cobertura e)
                (cylinder b espessura-diagonais-base-cobertura e)))))
  ;Procedimento que constroi recursivamente todas as faces.
  (define (constroi-base-cobertura lista-pontos)
    (cond
      ((not (null? (cdr lista-pontos)))
       (seccao-base (car lista-pontos) (car (cdr lista-pontos)))
       (constroi-base-cobertura (cdr lista-pontos)))))
  (constroi-base-cobertura lista-base-cobertura))