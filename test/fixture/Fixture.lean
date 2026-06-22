example : 1 = 1 := by
  rfl

theorem foo (a b : Nat) (h : a = b) : b = a := by
  exact h.symm

example : True := by
  sorry

theorem pending (a b : Nat) (h : a = b) : b = a := by
  sorry
