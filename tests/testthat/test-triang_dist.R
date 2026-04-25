#PRUEBAS PARA dtriang

test_that("dtriang lanza errores de seguridad correctamente", {
  # Caso 1: Mínimo mayor que máximo
  expect_error(dtriang(x = 1, min = 5, max = 2, mode = 3))

  # Caso 2: Moda por encima del máximo
  expect_error(dtriang(x = 1, min = 0, max = 5, mode = 10))

  # Caso 3: Moda por debajo del mínimo
  expect_error(dtriang(x = 1, min = 0, max = 5, mode = -2))
})

test_that("dtriang calcula bien las matemáticas", {
  # Caso 4: 'x' está fuera del triángulo por la izquierda (x < min)
  expect_equal(dtriang(-1, min = 0, max = 10, mode = 5), 0)

  # Caso 5: 'x' está fuera del triángulo por la derecha (x > max)
  expect_equal(dtriang(15, min = 0, max = 10, mode = 5), 0)

  # Caso 6: 'x' está en la cuesta de subida (x < mode)
  expect_equal(dtriang(2.5, min = 0, max = 10, mode = 5), 0.1)

  # Caso 7: 'x' está justo en la punta (x == mode)
  expect_equal(dtriang(5, min = 0, max = 10, mode = 5), 0.2)

  # Caso 8: 'x' está en la cuesta de bajada (x > mode)
  expect_equal(dtriang(7.5, min = 0, max = 10, mode = 5), 0.1)
})
#PRUEBAS PARA ptriang
test_that("ptriang lanza errores y calcula bien", {
  # Errores
  expect_error(ptriang(q = 5, min = 5, max = 2, mode = 3))
  expect_error(ptriang(q = 5, min = 0, max = 5, mode = 10))

  # Cálculos (0 por debajo, 1 por encima, y el área correcta en medio)
  expect_equal(ptriang(-1, min = 0, max = 10, mode = 5), 0)
  expect_equal(ptriang(15, min = 0, max = 10, mode = 5), 1)
  expect_equal(ptriang(5, min = 0, max = 10, mode = 5), 0.5)
})

# PRUEBAS PARA qtriang
test_that("qtriang lanza errores y calcula bien", {
  # Errores
  expect_error(qtriang(p = 0.5, min = 5, max = 2, mode = 3))
  expect_error(qtriang(p = 0.5, min = 0, max = 5, mode = 10))
  expect_error(qtriang(p = 1.5, min = 0, max = 10, mode = 5)) # Probabilidad > 1
  expect_error(qtriang(p = -0.5, min = 0, max = 10, mode = 5)) # Probabilidad < 0

  # Cálculos (La mediana)
  expect_equal(qtriang(0.5, min = 0, max = 10, mode = 5), 5)
})

#PRUEBAS PARA rtriang
test_that("rtriang lanza errores y genera vectores correctos", {
  # Errores
  expect_error(rtriang(n = 5, min = 5, max = 2, mode = 3))
  expect_error(rtriang(n = 5, min = 0, max = 5, mode = 10))
  expect_error(rtriang(n = -5, min = 0, max = 10, mode = 5)) # Cantidad negativa

  # Cálculos (Verificamos que genera la cantidad correcta de números aleatorios)
  expect_length(rtriang(10, min = 0, max = 10, mode = 5), 10)
  expect_length(rtriang(c(1,2,3), min = 0, max = 10, mode = 5), 3) # Comprobación de vector
})
