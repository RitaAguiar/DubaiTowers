;Gabriela Guimarães Ruggeri nº75610
;Rita Roldão de Aguiar nº75921

;;;;;;;CORPO DA COBERTURA

;Cria uma unidade do corpo da cobertura

(define (corpo-cobertura cota-unid unid au transf codigo u-i u-f c-i c-f lista-dados)
  ;Cria um identificador simples que constitui a função das coordenadas do ponto no referencial da unidade e as
  ; transforma nas coordenadas do referencial exterior
  (define t (transf cota-unid))
  ;Cria uma das faces do corpo da cobertura
  (define (face a-base b-base)
    ;Obtenção das coordenadas dos pontos no referencial exterior.
    (define a (t a-base))
    (define b (t b-base))
    (define c (t (+c b-base (xyz 0 0 au))))
    (define d (t (+c a-base (xyz 0 0 au))))
    (define a0 (t (+c a-base (xyz 0 0 (- au)))))
    (define d1 (t (+c a-base (xyz 0 0 (* 2 au)))))
    ;O tipo de desenho depende do código selecionado.
    (case codigo
           ((1)
            ;Coloca a travessa horizontal na base da face em wireframe
            (line a b c (projecao-ponto a b c d) a)))
    (case codigo
           ((2 3)
            ;Coloca a travessa horizontal na base da face em volume
            (with-current-layer "Aço" (cylinder a espessura-travessas-cobertura b))))
    (case codigo
           ((3)
            ;Coloca os vidros na face.
            ;Devido a restrições impostas pelos backend apresentam-se soluções diferentes para cada um deles
            (case backend-usado
              ((0)
               (with-current-layer "Vidro" (extrusion (superficie-projecao-a a b c d) espessura-vidros-cobertura)))
              ((1)
               (with-current-layer "Vidro" (thicken (superficie-projecao-r a b c d) espessura-vidros-cobertura))))))
    ;Coloca as travessas paralelas nas janelas da face.
    (coloca-travessas a b c (projecao-ponto a b c d) distancia-travessas-vidros espessura-travessas-vidros codigo))
  ;Procedimento que constroi recursivamente todas as faces.
  (define (constroi-unidade-cobertura lista-pontos)
    (cond
      ((not (null? (cdr lista-pontos)))
       (face (car lista-pontos) (car (cdr lista-pontos)))
       (constroi-unidade-cobertura (cdr lista-pontos)))))
  (constroi-unidade-cobertura lista-base-cobertura))