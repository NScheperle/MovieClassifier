def ost_login(ost):
	user = "MCDukeMIDS"
	pwd = "MovieClassifying"
	token = ost.login(user, pwd)
	return token