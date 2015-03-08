## script to run all tests

library('RUnit')

source('T.R')

test.suite <- defineTestSuite("Unit Test for T.R",
                              dirs = file.path("./tests"),
                              testFileRegexp = '^test_[A-Za-z0-9]*\\.R$',
                              testFuncRegexp = "^test_[A-Za-z0-9]*")

test.result <- runTestSuite(test.suite)

printTextProtocol(test.result)
