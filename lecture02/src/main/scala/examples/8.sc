// опасная штука
// без передачи по имени!!!! это другое
// нужно хорошо подумать где это использовать

// lazy
val x = { println("x"); 15 }
lazy val y = { println("y"); 13 }
println("z")
y
