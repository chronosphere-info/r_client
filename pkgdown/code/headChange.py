import sys
filename=sys.argv[1]
print(filename)

with open(filename, 'r') as file :
  filedata = file.read()

# Replace the target string
filedata = filedata.replace('<a class="navbar-brand me-2" href="index.html">chronosphere</a>',
                            '''<a class="navbar-brand me-2" href="https://www.chronosphere-portal.org/" style="padding-right:8px">
		  <img style="margin-right:8px" src="images/chronosphere_luminosity_500_hover.png" width="30px" height="30px">chronosphere</a> ›  
		<a style="margin-left:16px" class="navbar-brand" href="index.html"> <img style="margin-right:6px;position: relative; top: -2px;" src="images/r_luminosity_hover.png" width="25px" height="25px">client </a>''')

filedata = filedata.replace('<a class="navbar-brand me-2" href="../index.html">chronosphere</a>',
                            '''<a class="navbar-brand me-2" href="https://www.chronosphere-portal.org/" style="padding-right:8px">
		  <img style="margin-right:8px" src="../images/chronosphere_luminosity_500_hover.png" width="30px" height="30px">chronosphere</a> ›  
		<a style="margin-left:16px" class="navbar-brand" href="../index.html"> <img style="margin-right:6px;position: relative; top: -2px;" src="../images/r_luminosity_hover.png" width="25px" height="25px">client </a>''')

# Write the file out again
with open(filename, 'w') as file:
  file.write(filedata)
