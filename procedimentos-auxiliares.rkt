;Gabriela Guimarães Ruggeri nº75610
;Rita Roldão de Aguiar nº75921

;Para ser incluído no código sempre que se quer gravar uma imagem.
;Permite fazê-lo através de dois métodos.

(define (grava)
  (case gravacao
    ((1) (set! frame (+ frame 1))
         (render-view (format string-imagem frame)))
    ((2) (save-film-frame))))

;Coloca uma série de travessa paralelas num quadrilátero, não necesariamente regular.
;Criado para colocar as travessas nas janelas de vidro da base e do corpo da cobertura.

(define (coloca-travessas a b c d dist-entre-trav espessura-travessas-vidros codigo)
  ;Só constrói as travessa quando a distância especificada entre elas é diferente de 0
  ; e a distância entre os pontos base das janelas é diferente de 0.
  (cond ((and (not (= dist-entre-trav 0)) (not (= (distance a b) 0)))
         ;Cálculos matemáticos
         (define ab (-c b a))
         (define ad (-c d a))
         (define m (ponto-medio a b))
         (define r-a (- (distance a m)))
         (define p-d (/ (produto-escalar ab ad) (distance a b)))
         (define r-d (+ r-a p-d))
         (define prim r-a)
         (cond ((> p-d 0) (set! prim r-d)))
         (define bc (-c c b))
         (define dc (-c c d))
         (define r-b (distance b m))
         (define p-c (/ (produto-escalar ab bc) (distance a b)))
         (define r-c (+ r-b p-c))
         (define ult r-c)
         (cond ((> p-c 0) (set! ult r-b)))
         (define vs-ab (/c ab (distance a b)))
         (define v-det-i (*c vs-ab dist-entre-trav))
         (define coef (/ (distance d c) (- r-c r-d)))
         (define vs-dc (/c dc (distance c d)))
         (define v-det-s (*c vs-dc (* dist-entre-trav coef)))
         (define m-s (+c d (*c vs-dc (* (abs r-d) coef))))
         (define inic (exact-ceiling (/ prim dist-entre-trav)))
         ;Procedimento que constrói recursivamente de intervalos regulares uma sucessão de 
         ; travessa paralelas.
         (define (itera-travessas i)
           (cond
             ((<= (* i dist-entre-trav) ult)
              (case codigo
                ((1)
                 (line (+c m (*c v-det-i i)) (+c m-s (*c v-det-s i)))))
              (case codigo
                ((2 3)
                 (with-current-layer "Aço" (cylinder (+c m (*c v-det-i i)) espessura-travessas-vidros (+c m-s (*c v-det-s i))))))
              (set! frame (+ frame 1))
              (itera-travessas (+ i 1) ))))
         (itera-travessas inic))))

