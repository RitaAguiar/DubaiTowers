;Gabriela Guimarães Ruggeri nº75610
;Rita Roldão de Aguiar nº75921

;Cria uma unidade do topo da cobertura

(define (topo-cobertura cota-unid unid au transf codigo u-i u-f c-i c-f lista-dados)
  ;Cria um identificador simples que constitui a função das coordenadas do ponto no referencial da unidade e as
  ; transforma nas coordenadas do referencial exterior
  (define t (transf cota-unid))
  ;Cria uma das faces do top da cobertura
  (define (face a-base b-base)
    (define a (t a-base))
    (define b (t b-base))
    (define c (t (+c b-base (xyz 0 0 au))))
    (define d (t (+c a-base (xyz 0 0 au))))
    (define a0 (t (+c a-base (xyz 0 0 (- au)))))
    (define d1 (t (+c a-base (xyz 0 0 (* 2 au)))))
    ;O tipo de desenho depende do código selecionado
    (case codigo
           ((1)
            (line a b)))
    (case codigo
           ((2 3)
            ;Coloca a travessa horizontal na base da face em wireframe
            (with-current-layer "Aço" (cylinder a espessura-travessas-topo b))))
    (case codigo
           ((3)
            ;Coloca os vidros nas janelas
            (case backend-usado
              ((0)
               ;Coloca os vidros na face.
               ;Devido a restrições impostas pelos backend apresentam-se soluções diferentes para cada um deles
               (cond
                 ((< unid u-f)
                  (with-current-layer "Vidro" (extrusion (superficie-projecao-a a b c d) espessura-vidros-cobertura)))
                 (else
                  (with-current-layer "Vidro" (extrusion (surface (polygon a b c)) espessura-vidros-cobertura))
                  (with-current-layer "Vidro" (extrusion (surface (polygon a b d)) espessura-vidros-cobertura)))))
              ((1)
                (with-current-layer "Vidro" (thicken (superficie-projecao-r a b c d) espessura-vidros-cobertura)))))))
  ;Procedimento que constroi recursivamente todas as faces.
  (define (constroi-unidade-cobertura lista-pontos)
    (cond
      ((not (null? (cdr lista-pontos)))
       (face (car lista-pontos) (car (cdr lista-pontos)))
       (constroi-unidade-cobertura (cdr lista-pontos)))))
  (constroi-unidade-cobertura lista-base-cobertura))
