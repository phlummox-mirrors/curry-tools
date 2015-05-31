-- Example with multiple guards:

negOrPos x | x<0 = "Neg"
           | x>0 = "Pos"
	   