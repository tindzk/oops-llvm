#!/usr/bin/env python
import fnmatch, os, sys, subprocess

def run(cmd):
	try:
		p = subprocess.Popen(cmd)
		(out, err) = p.communicate()

		if out:
			print('out: %s' % out.decode(sys.stdout.encoding or 'iso8859-1'))

		if err:
			print('err: %s' % err.decode(sys.stdout.encoding or 'iso8859-1'))

		return p.returncode
	except OSError:
		return -1

def matchFiles(path, pattern):
	paths = []
	for root, dirnames, filenames in os.walk(path):
		for filename in fnmatch.filter(filenames, pattern):
			paths.append(os.path.join(root, filename))
	return paths

jarFiles = matchFiles('libs/', '*.jar')
jarFiles += ['/usr/share/scala/lib/scala-library.jar',
		'/usr/share/scala/lib/scala-reflect.jar']

if not os.path.exists("build/"):
	os.makedirs("build/")

javaFiles = matchFiles('src/', '*.java')

scalaFiles = matchFiles('src/', '*.scala')
scalaFiles = [file for file in scalaFiles if "TestSuite.scala" not in file]

srcFiles = javaFiles + scalaFiles

cmdCompile = ["/usr/bin/scalac", "-cp", ":".join(jarFiles), "-d", "build/"] + srcFiles
print(" ".join(cmdCompile))
run(cmdCompile)

cmdCompile = ["/usr/bin/javac", "-cp", ":".join(jarFiles) + ":build/", "-d", "build/"] + javaFiles
print(" ".join(cmdCompile))
run(cmdCompile)

classFiles = []
for file in matchFiles('build/', '*.class'):
	classFiles.append("-C")
	classFiles.append("build/")
	classFiles.append(file.replace("build/", ""))

cmdPackage = ["/usr/bin/jar", "cvmfe", "Manifest.txt", "oopsc.jar", "org.oopsc.OOPSC"] + classFiles
print(" ".join(cmdPackage))
run(cmdPackage)
