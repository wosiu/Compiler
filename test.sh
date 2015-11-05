for f in `ls --format=single-column -1 tests/*.ins`
do
	pref=$(echo $f | cut -d'.' -f 1)
	name=$(echo $pref | cut -d'/' -f 2)
	eval=$pref."eval"
	#echo "pref: "$pref
	#echo "name: "$name
	#echo "eval: "$eval
	./insc_jvm $f
	java -classpath tests/ $name > tmp_jvm.out
	diff -sq -Bb tmp_jvm.out $eval || break	
	./insc_llvm $f
	lli $pref".bc" > tmp_llvm.out
	diff -sq -Bb tmp_llvm.out $eval || break
done


