call(call(
  2
))

call(call(1,
  2))
# multi-line: no indention based on first vall
call(a(b(c({
}))))

call(call(
  2),
5)


call(call(1,
  2, c(
    3
)))

call(1,
  call2(3, 4, call(3,
    4, call(5, 6, call(
      2
    )
    )
  )
  )
)

# comment lala

call(call(
  2
))

call(1, call(
  23
))
