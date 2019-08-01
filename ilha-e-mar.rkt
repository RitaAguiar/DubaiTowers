;Gabriela Guimarães Ruggeri nº75610
;Rita Roldão de Aguiar nº75921

(define (ilha-e-mar)
  (case codigo-ilha-e-mar
    ((1)
     (with-current-layer "Ilha"
                         (extrusion (surface (curva-superelipse (xy 0 400) 600 800 2 1000)) 5))
     (with-current-layer "Mar"
                         (extrusion (surface (curva-superelipse (xy 0 400) 1200 1600 2 1000)) -15)))))