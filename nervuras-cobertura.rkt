;Gabriela Guimarães Ruggeri nº75610
;Rita Roldão de Aguiar nº75921

;Cria as nervuras de uma só vez entre as cotas especificadas

(define (nervuras-cobertura cota-unid unid aut transf codigo u-i u-f c-i c-f lista-dados)
  ;Cria um identificador simples que constitui a função das coordenadas do ponto no referencial da unidade e as
  ; transforma nas coordenadas do referencial exterior
  (define t (transf cota-unid))
  ;Quando o pontobase não se encontra à cota 0, é necssário criar estes limites para não desenhar fora das cotas especificadas
  (define cota-inferior (max 0 c-i))
  (define cota-superior (min c-f altura-torre))
  ;Procedimento que cria uma nervura a partir do ponto base dado entre as cotas especificadas
  (define (cria-nervura ponto-base-nervura)
    (define lista-pontos-nervura
      (map-division (lambda (cota) (t (+c ponto-base-nervura (xyz 0 0 cota))))
                   cota-inferior
                   cota-superior
                   (* 10 (exact-ceiling (/ (- cota-superior cota-inferior) aut)))))
    ;O tipo de desenho depende do código selecionado
    (case codigo
         ((1)
          ;Cria uma nervura em wireframe
          (spline  lista-pontos-nervura)))
    (case codigo
         ((2 3)
          ;Cria uma nervura em volume
          (with-current-layer "Aço"
                (sweep (spline lista-pontos-nervura)
                 (surface-circle (xy 0 0) espessura-nervuras-cobertura))))))
  ;Cria uma nervura para cada ponto base
  (map cria-nervura lista-base-cobertura))
