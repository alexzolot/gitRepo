# TODO: Add comment
# 
# Author: alexzol
###############################################################################
source('M/83_ScopeR/ScopeR.r')

	
	
	#list of functions for package
	rmall()
	ls()
	getwd()	# [1] sw("m:/80_RPack/zBase")

	dir()	# [1] "inst"                    "man"                     "zmFunctionsForPackage.r"
	source("zmFunctionsForPackage.r")
	source("M:/83_ScopeR/ScopeR.r")
	
	x= lss()
	pas(sort(na(x)),,' ')
	f.base= "%-% %+% ab brr catf catt ch cn d.o df dhe dsh evall expl fa fDate fid fsize gcc gf gtf gw Hd he hee heta HH hLink Hp Ht HT HTMLp isNum JaccardSimilarity JS LastDayOfWeek le libra libras LiftWtArr lo logg logit lsDF lss lss0 ma me Model mt na newwin nin nmsv nope norm nu nut one ord ord2 ordd pas pr prr renamee rmall rmDF rows.with.na rt rtd rtsv sa sf showInOpera sNames sort.data.frame sortt spectr srt st strReverse summ summar sus sw tab Timer tocsv toXL ToXL tra trb wc week weighted.var zeval zHTML zlog10 zlogit"
    f1= "dir.scope dir.TFS DownloadScopeFiles file.show.TFS GetScriptParams LastDayOfWeek LastSaturdayOfWeek NamesFromScript nope PrepReadScript3 ReadAllTSV RunJob RunJob0 scope ScopeDT transform.scopeScript wc week"
 	f2= f1 %-% f.base
	f.zRScope= cn("dir.scope dir.TFS DownloadScopeFiles file.show.TFS GetScriptParams
			 LastDayOfWeek LastSaturdayOfWeek NamesFromScript  PrepReadScript3 
		  	 ReadAllTSV RunJob RunJob0 scope ScopeDT transform.scopeScript wc week")
	
package.skeleton("zBase"
		, cn("`%-%` `%+%` ab brr catf catt ch cn d.o df dhe dsh evall expl fa 
						fDate fid fsize gcc gf gtf gw Hd he hee heta HH hLink Hp Ht HT HTMLp isNum 
						JaccardSimilarity JS LastDayOfWeek le libra libras LiftWtArr 
						lo logg logit lsDF lss lss0 ma me Model mt na newwin nin nmsv nope norm 
						nu nut one ord ord2 ordd pas pr prr renamee rmall rmDF rows.with.na 
						rt rtd rtsv sa sf showInOpera sNames sort.data.frame sortt spectr srt st strReverse 
						summ summar sus sw tab Timer tocsv toXL ToXL tra trb wc week weighted.var 
						zeval zHTML zlog10 zlogit")) 
if(0){
	debug(sw)
	file.exists('M:/79_RPack/zBase/out/zBase/R')
	sw("M:/79_RPack/zBase/out/zBase/R/")
	dir.create('out2')
	package.skeleton("zBase",  code_files="zBase.r", namespace = TRUE, path='out2') 
	

	# again =====================================================
	rmall()
	source('M:/79_RPack/zBase/out/zBase/R/zBase.r')
	x1= ls()
	source('M:/83_ScopeR/ScopeR.r')
	x12= ls()
	x2= x12 %-% x1
	pas(x2,collapse=' ')
	# [1] "dir.scope dir.TFS DownloadScopeFiles file.show.TFS GetScriptParams LastSaturdayOfWeek NamesFromScript PrepReadScript3 ReadAllTSV RunJob RunJob0 scope ScopeDT transform.scopeScript Upload2Scope x1"

	pas(x1,collapse=' ')
	# [1] "%-% %+% ab brr catf catt ch cn d.o df dhe dsh evall exec execf expl fa fDate fid fp fsize gcc gf gtf gw Hd he hee heta HH hLink Hp Ht HT HTMLp isNum JaccardSimilarity JS LastDayOfWeek le libra libras LiftWtArr lo logg logit lsDF lss lss0 ma me Model mt na newwin nin nmsv nope norm nu nut one ord ord2 ordd pas pr prr renamee rmall rmDF rows.with.na rt rtd rtsv sa sf showInOpera sNames sort.data.frame sortt spectr srt st strReverse summ summar sus sw tab Timer tocsv toXL ToXL tra trb wc week weighted.var zeval zHTML zlog10 zlogit"
x11= cn('ab brr catf catt ch cn d.o df dhe dsh evall exec execf expl fa fDate fid 
fp fsize gcc gf gtf gw Hd hee heta HH hLink Hp Ht HT HTMLp isNum 
JaccardSimilarity JS LastDayOfWeek le libra libras LiftWtArr lo logg logit lsDF 
lss lss0 ma me Model mt na newwin nin nmsv nope norm nu nut one ord ord2 ordd 
pas pr prr renamee rmall rmDF rows.with.na rt rtd rtsv sa sf showInOpera 
sNames sort.data.frame sortt spectr srt st strReverse summar sus sw tab Timer tocsv 
toXL ToXL tra trb wc week weighted.var zeval zHTML zlog10 zlogit')
	

package.skeleton("zBase", x11, namespace = TRUE, path='M:/79_RPack/outBase2') 
package.skeleton("zBase",  code_files="M:/79_RPack/zBase/out/zBase/R/zBase.r", namespace = TRUE, path='M:/79_RPack/outBase') 
package.skeleton("ScopeR",  code_files="M:/83_ScopeR/ScopeR.r", namespace = TRUE, path='M:/79_RPack/outScopeR') 
package.skeleton("ScopeR",  x2, namespace = TRUE, path='M:/79_RPack/outScopeR2') 

package.skeleton("zBase2", code_files="R:/R-2.14.0/library/zBase/R/zBase2.R",  namespace = TRUE, path='M:/79_RPack/outScopeR2') 
package.skeleton("zBase2", code_files="R:/R-2.14.0/library/zBase/R/zBase2.R",  namespace = TRUE, path='M:/79_RPack/outScopeR2') 
package.skeleton("zBase3", code_files="r:/work/R-proj/zBase3.R",  namespace = TRUE, path='M:/79_RPack/outScopeR2') 

root= 'M:/79_RPack/outScopeR2/zBase2'
root= 'M:/79_RPack/outScopeR2/zBase3'
root= 'M:/79_RPack/outScopeR2/zBase'
root= 'M:/79_RPack/outScopeR3/Scoper'
expl(root)
sw(root)#  chr "M:/79_RPack/outScopeR2/zBase2"
#  chr "M:/79_RPack/outScopeR3/Scoper"


#check 
prr(execf('R CMD check %s', root))
fef('M:/79_RPack/outScopeR3/Scoper/Scoper.Rcheck/', '00install.out')

prr(execf('R CMD build --binary --use-zip %s', gsub('/','\\\\',root)))
browseURL('http://win-builder.r-project.org/upload.aspx')

##zzzz

prr(execf('R CMD build %s', gsub('/','\\\\',root)))
.libPaths() # [1] "R:/R-2.14.0/library"

install.packages(sf('%s/zBase2_1.0.tar.gz', root), repos= NULL) 
#nOK
install.packages(sf('%s/zBase2_1.0.tar/zBase2.zip', root), repos= NULL) 
install.packages(sf('%s/zBase2_1.0/zBase2.zip', root), repos= NULL) 
install.packages(sf('%s/zBase2_1.0.zip', root), repos= NULL) 

install.packages(sf('%s/zBase_0.1.zip', root), repos= NULL) 
install.packages(sf('%s/ScopeR_0.1.zip', root), repos= NULL) 

expl(.libPaths())

}
package.skeleton("RScope"
			, f.zRScope,path='M:/83_ScopeR/RPack2') 
	#Further steps are described in './zBase/Read-and-delete-me'.
	#Further steps are described in 'M:/83_ScopeR/RPack2/RScope/Read-and-delete-me'.
gw()# [1] "m:/80_RPack/zBase/out"
expl('./zBase/')
expl('M:/83_ScopeR/RPack2/RScope')

zzz


		libra(roxygen2)
		roxygenise('m:/80_RPack/zBase/out/zBase', 'm:/80_RPack/zBase/out')

        root='M:/80_RPack/zBase/out/zBase'
		root='M:/79_RPack/zBase/out/zBase'
		
		root='M:/79_RPack/outScoper/ScopeR'
		root='M:/79_RPack/outScoper2/ScopeR'
		root='M:/79_RPack/outBase/zBase'
		root='M:/79_RPack/outBase2/zBase'
		
		sw(root)

		#crea txt for all Rd
		for(f in dir(sf('%s/man', root), patt='Rd$')){
			(shell(sf('R CMD Rd2txt %s/man/%s --output=%s/man/%s', root, f, root, gsub('Rd','txt',f), intern =T )))
			(shell(sf('R CMD Rd2pdf %s/man/%s --output=%s/man/%s', root, f, root, gsub('Rd','pdf',f), intern =T )))
			(shell(sf('R CMD Rdconv -t html %s/man/%s --output=%s/man/%s', root, f, root, gsub('Rd','html',f), intern =T )))
		}	
	
		#check 
		sh= execf('R CMD check %s', root)
		prr(sh)
		
		#build ==
        sh= shell(sf('R CMD build  --binary --use-zip %s', gsub('.*/','',root)), intern =T )
        sh= shell(sf('R CMD build  --binary --use-zip %s', gsub('/','\\\\',root)), intern =T )
		
		sh= execf('R CMD build  --binary --use-zip %s', gsub('/','\\\\',root))
		sh= execf('C:\\Progra~1\\R\\R-2.13.2\\bin\\x64\\R.exe CMD build --binary --use-zip %s', gsub('/','\\\\',root))
		prr(sh)
		
		install.packages(sf('%s/../zBase_1.0.zip', root), repos =NULL) 
		libra(zBase)
		?catf		
		
		
		
	
	
	
#=====================================================
	gw()	    # [1] expl("C:/z/exe/eclipse")

    package.skeleton("zFunc", c("%+%",cn("catf catt ch fDate gw he sNames le libra logg lsDF nu pr sf sus sw wc")) )
    
    root= 'M:/83_ScopeR/RPackaging/ScopeR'
    
    
    root= "C:/z/exe/eclipse/zFunc"
    dir(sf('%s/man', root), patt='Rd$')
    
#crea titles for all Rd
    for(f in dir(sf('%s/man', root), patt='Rd$')){
        s= readLines(sf('%s/man/%s',root, f), warn=F)
        s= gsub('(title\\{)$',sf('\\1 Function %s.',gsub('Rd','',f)), s)
        print(s[3:4])
        writeLines(he(s,9999), con = sf('%s/man/%s',root, f))
    }    
    
    #comment all examples for all Rd
    for(f in dir(sf('%s/man', root), patt='Rd$')){# f='dir.scope.Rd'
        flag=0
        s= readLines(sf('%s/man/%s',root, f), warn=F)
        i=  min(which(grepl('examples\\{',s)))
        s[i:le(s)]= '%%-   ' %+%  s[i:le(s)]
        writeLines(he(s,9999), con = sf('%s/man/%s',root, f))
    }    
    
    #crea txt for all Rd
    for(f in dir(sf('%s/man', root), patt='Rd$')){
         (shell(sf('R CMD Rd2txt %s/man/%s --output=%s/man/%s', root, f, root, gsub('Rd','txt',f), intern =T )))
         (shell(sf('R CMD Rd2pdf %s/man/%s --output=%s/man/%s', root, f, root, gsub('Rd','pdf',f), intern =T )))
     }
    
    
    for(f in dir(sf('%s/man', root), patt='Rd$'))
        pr(gf('title\\{$', file.path('M:/83_ScopeR/RPackaging/ScopeR/man',f)))
        pr(gf('name', file.path('M:/83_ScopeR/RPackaging/ScopeR/man',f)))
    

        #check 
        sh= shell(sf('R CMD check %s', root), intern =T )
        for(s in sh)catt(s)
        
        #build ==
sh= shell(sf('R CMD build %s', root), intern =T )
sh= shell(sf('R CMD build %s', gsub('/','\\\\',root)), intern =T )
sh= shell(sf('R CMD build %s', gsub('.*/','',root)), intern =T )
# works only in cmd window

sw(root); sw('..');gw()
sh= shell(sf('R CMD build  --binary --use-zip %s', gsub('.*/','',root)), intern =T )
for(s in sh)catt(s)

install.packages('C:/z/exe/eclipse/zFunc_1.0.zip', repos =NULL) 
libra(zFunc)
?catf

install.packages('M:/83_ScopeR/RPackaging/ScopeR_1.0.zip', repos =NULL) 
libra(ScopeR)
?dir.scope
expl('Z:/exe/R-2.13.0-port/library/ScopeR/help')



root= 'M:/83_ScopeR/testdir/'
sh= shell(sf('R CMD build  --binary --use-zip %s', gsub('.*/','',root)), intern =T )
for(s in sh)catt(s)

install.packages('C:/z/exe/eclipse/zFunc_1.0.zip', repos =NULL) 
libra(zFunc)


	gw()	# [1] "M:/83_ScopeR/testdir/10-21.1d-5"
    package.skeleton("ScopeR", cn("scope week LastSaturdayOfWeek dir.scope RunJob ReadAllTSV 
            GetShivaParams  transform.scopeScript PrepReadScript3 file.show.TFS dir.TFS DownloadScopeFiles"))
	
	gf('fun',"M:/83_ScopeR/ScopeR.r")
	# 3099 ============================ gtf: M:/83_ScopeR/ScopeR.r
	#    6. scope= function(cmd='-submit', ScopeFolder='m:/scope1', vc='http://cosmos05.osdinfra.net:88/cosmos/adCenter.BICore.Fraud',...) {
	#   18. week= function(Date= Sys.Date())as.numeric(Date - as.Date('2011-01-01'),"days")  %/% 7  +1
	#   21. LastSaturdayOfWeek= LastDayOfWeek= function(Week= week(Sys.Date()-6))  as.Date('2011-01-01')  + 7 * Week
	#   24. dir.scope= function(ScopeFolder = "m:/Scope1"
	#   49. RunJob0= function(TheCodeFolder= 'M:/93_AegisPubScore', ScopeFolder = "m:/Scope1", msalias='alexzol', vc="http://cosmos04.osdinfra.net:88/cosmos/adCenter.BICore.Fraud"
	#   61. 		SetState= function(s){dput(state<<- s, file="state.txt"); catt('RunJob: SetState: ==== State set to ', state); logg('== State set to ', state); state} 
	#   62. 		GetState= function(){ if(!file.exists('state.txt'))SetState('Init')
	#  315. RunJob= function(LocalRoot= 'M:/test', CosmosRoot= "/my/test", ScopeFolder = "m:/Scope1", msalias='alexzol', vc="http://cosmos04.osdinfra.net:88/cosmos/adCenter.BICore.Fraud"
	#  334. 	dump= lapply(varnames, function(x)eval(parse(text= x)))
	#  356. 	logg= function(...){catt(...); cat(format(Sys.time(), "%Y-%b-%d %H:%M:%S "),..., '\n', file = "log.txt", append = T)}
	#  357. 	loggState= function(s,State){catt('loggState:',s);str(State);
	#  362. 	#SetState= function(s){dput(State<<- s, file="State.txt"); catt('RunJob: SetState: ==== State set to ', str(State)); logg('== State set to ', str(State)); State} 
	#  363. 	SetState= function(s, change= T){
	#  373. 	GetState= function(){ if(!file.exists('State.txt'))SetState(list(state='Init', dump=dump, LocJobSubdir=LocJobSubdir), change=F)
	#  607. 	testRep= function(State) {
	#  624. #ReadAllTSV= function(folder=gw(), patt='\\..+\\.tsv', sep = "\t", row.max= 800000, MaxLen=1e8){
	#  625. ReadAllTSV= function(folder='M:/87_AegPubScor2/test', patt='\\..+\\.csv'
	#  631.                rows= nu(sapply(strsplit(sapply(file.path(folder, f), wc), ' '),function(x)x[1])), size=file.info(file.path(folder, f))$size); print(ff)
	#  702. GetShivaParams= function(script="M:/83_ScopeR/AegisCustomDataSourceView.script",
	#  743. transform.scopeScript= function(script="M:/83_ScopeR/AegisCustomDataSourceView.script", fileShow= F
	#  756.     for(key in names(args)) s= gsub(sf("@%s\\b", key), eval(args[[key]], envir= sys.parent()), s) # environment(fun = RunJob) parent.frame()
	#  831. PrepReadScript3= function(script="M:/93_AegisPubScore/93_ConvDownlift.script",
	#  871.     ldply(dsname, function(x){y=-1; try({y=nrow(get(x, envir=.GlobalEnv))},s=T);y})
	#  928. nope= function(){
	#  969. file.show.TFS= function(TFSpath, fname, outDir= tempdir(), fShow=T){
	#  986. dir.TFS= function(TFSdir){
	# 1096.     ldply(dir('M:/84_700badIPs'), function(fname) df(fname,file.info(file.path('M:/84_700badIPs',fname))))
	# 1101. DownloadScopeFiles= function(ScopeFolder='m:Scope'
	# 1133.         ff= ldply(dir(LocDir), function(fname) df(fname, file.info(file.path(LocDir,fname))))

`before  R CMD built  execute in shell===`
## before  R CMD built  execute in shell:
#!	set pp=%path%
#!	echo %pp%
#!	set path=C:\Rtools\bin;C:\Rtools\MinGW\bin;C:\cygwin\bin;C:\Program Files (x86)\MiKTeX 2.8\miktex\bin;C:\z\exe\R-2.13.0-port\bin;C:\Program Files (x86)\HTML Help Workshop;%pp%
#!	echo %path%

# M:\83_ScopeR\testdir\10-21.1d-5>r:\R-2.13.0-port\bin\R.exe R CMD build ScopeR
# M:\83_ScopeR\RPackaging>R CMD build ScopeR
#	        
#	http://yusung.blogspot.com/2006/12/making-r-packages-under-windows_10.html
#	check:
#!	gcc --help
#!	perl --help
#!	TeX --help
#!	R CMD --help

dir('M:/83_ScopeR/RPackaging/ScopeR/man', patt='Rd$')

#crea txt for all Rd
for(f in dir('M:/83_ScopeR/RPackaging/ScopeR/man', patt='Rd$'))
    (shell(sf('R CMD Rd2txt M:\\83_ScopeR\\RPackaging\\ScopeR\\man\\%s --output=M:\\83_ScopeR\\RPackaging\\ScopeR\\man\\%s', f,gsub('Rd','txt',f), intern =T )))

for(f in dir('M:/83_ScopeR/RPackaging/ScopeR/man', patt='Rd$'))
    pr(gf('title\\{$', file.path('M:/83_ScopeR/RPackaging/ScopeR/man',f)))
   pr(gf('name', file.path('M:/83_ScopeR/RPackaging/ScopeR/man',f)))
 
#crea titles for all Rd
for(f in dir('M:/83_ScopeR/RPackaging/ScopeR/man', patt='Rd$')){
    s= readLines(file.path('M:/83_ScopeR/RPackaging/ScopeR/man',f), warn=F)
    s= gsub('(title\\{)$',sf('\\1 Function %s.',gsub('Rd','',f)), s)
    print(s[3:4])
    writeLines(he(s,9999), con = file.path('M:/83_ScopeR/RPackaging/ScopeR/man',f))
}

#! M:\83_ScopeR\RPackaging>R CMD check ScopeRA


# missing functions
gf('no visible global function definition', 'M:/83_ScopeR/RPackaging/ScopeR.Rcheck/00check.log')
 s= readLines('M:/83_ScopeR/RPackaging/ScopeR.Rcheck/00check.log')
 ii= grep('no visible global function definition for', s)
 s1=  gsub("^.*(no visible global function definition for \\'(.+))\\'.*$", '\\2', s[ii])
 paste(sort(unique(s1)), collapse=' ')
 # [1] "%+% catf catt ch fDate gw he ldply le libra logg lsDF nu pr sf sus sw wc"


dump.frames()

# 1 min exercise ====
Timer(5,14)

 f='week.Rd';  pr(gf('title', file.path('M:/83_ScopeR/RPackaging/ScopeR/man',f)))
 

#! R CMD Rd2txt dir.scope.Rd
#! R CMD Rdconv -t=html -o=dir.scope.html dir.scope.Rd
#! 
#! R CMD Rd2dvi --pdf ScopeR
#! R CMD Rd2pdf *.rd
#! 
#! cd ..\..
#! R CMD check ScopeR

sessionInfo()
.libPaths()# [1] "Z:/exe/R-2.13.0-port/library"

library(ScopeR)
install.packages(ScopeR, )
install.packages('ScopeR',  repos ='M:/83_ScopeR/RPackaging/ScopeR_1.0.tar.gz')
install.packages('ScopeR',  repos ='M:/83_ScopeR/RPackaging/')
install.packages('M:/83_ScopeR/RPackaging/ScopeR_1.0.tar.gz', repos =NULL)
install.packages('M:/83_ScopeR/RPackaging/ScopeR_1.0.tar/ScopeR.zip', repos =NULL)

gw()

libra(roxygen2)
roxygenize('M:/83_ScopeR/RPackaging/ScopeR')
roxygenize('M:/83_ScopeR/1')
expl('M:/83_ScopeR/ScopeR.r')

libra(parseRd)
parse_Rd

fs= list(ls())
for(x in fs){catt(x, class(get(x, env=.GlobalEnv)))
	
	if(class(get(x, env=.GlobalEnv))== 'function')fs[[x]]=1
	sNames(names(fs))
	
}


`start to prep packaging` =0  ##########################################################
if(0){ 
	
	
	package.skeleton("ScopeR",  code_files="r:/work/R-proj/ScopeR.r"
	 , namespace = TRUE, path='M:/79_RPack/outScopeR3') 
	
    root= 'M:/79_RPack/outScopeR3/ScopeR'
	sw(root)
	expl(root)
	dir(root)
	# "DESCRIPTION"        "man"                "NAMESPACE"          "R"                  "Read-and-delete-me"

	
	file.edit(fp('R:/R-2.14.0/library/zBase',"DESCRIPTION")) #examples
	file.edit(fp('R:/R-2.14.0/library/ScopeR',"DESCRIPTION"))
	file.edit(fp(root,"DESCRIPTION")) # to edit
	gw()
	
	fef= function(...)file.edit(fp(...))
	
	fef('R:/R-2.14.0/library/zBase/man',"ab.Rd") #examples
	file.edit(fp(root,"man/ScopeR-package.Rd")) # to edit
	
	
	rmall()
	ls()
	#source('r:/work/R-proj/zBase.r')
	source('r:/work/R-proj/ScopeR.r')
	sw('r:/work/R-proj')
	
	fDump= "zBase3.R"
	fDump= "dump.S.R"	
	a= dump(ls(), file = fDump)
	
	#source('r:/work/R-proj/zBase.r') # add zBase to ls()
	libra(zBase) # add zBase to ls()
	
#prep aliases
	prr(sf('\\alias{%s}', a))
	
#prep usage & details

	s= readLines(fDump, warn=F)
	ii= grep('func',s, v=F)
	
#prep usage
	prr(sf('%s%s', gsub(' <-','',s[ii-1]), gsub('function','',gsub('\\{.*$','',s[ii]))))
	
#prep details
	prr(sf('%-15s   %s', s[ii-1], gsub('\\{.*$','',s[ii])))
	
	
	rmall()
	install.packages('M:/79_RPack/zBase_0.1/zBase_0.2.zip', repos =NULL)
	
	
	
	library(zBase)  # aliases and misc funcs
	
	?catt
	'a' %+% 'b'
}



#	IIS manager - Start server
#	C:\inetpub\wwwroot\1
#	http://10.123.100.42/1/gMotionChartPub.htm  (ipconfig /all -> IP address)
#	http://alexzol-msft/1/gMotionChartPub.htm


##== zBase-0.3 ===
##================
`2012-01-02   zBase-0.3`= {
	sw('M:/79_RPack/zBase_0.3/out1/zBase')
	# Work dir set to: M:/79_RPack/zBase_0.3/out1/zBase;  gw()


	file.copy("Z:/gitRepo/gitRepo/git1/git/zBase.b.R",'M:/79_RPack/zBase_0.3/zBase-0.3.R')
	# [1] TRUE




	package.skeleton("zBase",  code_files='M:/79_RPack/zBase_0.3/zBase-0.3.R', namespace = TRUE
			, path='M:/79_RPack/zBase_0.3/out1', force =T) 
	file.edit('M:/79_RPack/zBase_0.3/out1/zBase/Read-and-delete-me')
	expls()
	
	root= "M:/79_RPack/zBase_0.3/out1/zBase"
	dir(sf('%s/man', root), patt='Rd$')
	dir(sf('%s/man2', root), patt='Rd$')
	
		
		# http://www.stat.ucl.ac.be/ISdidactique/Rhelp/doc/keywords.html  R keywords
		# http://developer.r-project.org/Rds.html  Guidelines for Rd files
	
    #crea titles for all Rd
	for(f in dir(sf('%s/man0', root), patt='Rd$')){ catt(460,f)
		s= readLines(sf('%s/man0/%s',root, f), warn=F)
		#s[c(3,5:6,8:9,13)]= ''
		#s[grep('^\\%',s)]= ''
		s= gsub('(title\\{)$',sf('\\1 Function %s}',gsub('\\.Rd','',f)), s)
		s= gsub('.*description\\{$','', s)
		s= gsub('.*details\\{$','', s)
		s[c(1:9,13:le(s))]= gsub('^\\}$','', s[c(1:9,13:le(s))])  #excl  usage
		s[15:le(s)]= gsub('(.+\\{)$', '\\1 TBD}', s[15:le(s)])  #after argum
		s= gsub('^\\%.*$','', s) # drop comments
		s= gsub('dots\\}\\{$','dots}{other arguments to pass}', s)
		s= gsub('references\\{$', '', s)
		s= gsub('author\\{$', 'author\\{Alex Zolotovitski', s)
		s= gsub('note\\{$','', s)
		
		s= gsub('(.*~kwd1.*)', '}\n\\1', s) # insert { before kwd1 (after example)
		s= gsub('~kwd1', 'methods', s)
		s= gsub('~kwd2', 'programming', s)
		s= gsub('seealso\\{$','', s)
		print(s[3:4])
		
		s=s[s!= '']
		s=s[s!= '\\']
		s[grepl('TBD',s) & !grepl('examples',s)]= '  %' %+% s[grepl('TBD',s) & !grepl('examples',s)]

		s= gsub('(.*value.*)', '}\n\\1', s) # insert { before value (after argum)
		s= gsub('\\\\examples\\{', '\n\\\\examples{ % \\\\dontrun{} ', s)
		
		writeLines(he(s,9999), con = sf('%s/man/%s',root, f))
		#stop('aaaaa')
	}   
	
	
	#comment all examples for all Rd
	for(f in dir(sf('%s/man', root), patt='Rd$')){# f='dir.scope.Rd'
		flag=0
		s= readLines(sf('%s/man/%s', root, f), warn=F)
		i=  min(which(grepl('examples\\{',s)))
		s[i:le(s)]= '%%-   ' %+%  s[i:le(s)]
		writeLines(he(s,9999), con = sf('%s/man/%s',root, f))
	}  
	
#crea txt for all Rd
for(f in dir(sf('%s/man', root), patt='Rd$')){
	(shell(sf('R CMD Rd2txt %s/man2/%s --output=%s/man2/%s', root, f, root, gsub('Rd','txt',f), intern =T )))
	#(shell(sf('R CMD Rd2pdf %s/man/%s --output=%s/man/%s', root, f, root, gsub('Rd','pdf',f), intern =T )))
}

#check
sh= execf('R CMD check %s', root)
prr(sh)
file.edit('M:/79_RPack/zBase_0.3/out1/zBase/zBase.Rcheck/00install.out')

#build ==
sh= shell(sf('R CMD build  --binary --use-zip %s', gsub('.*/','',root)), intern =T )
sh= shell(sf('R CMD build  --binary --use-zip %s', gsub('/','\\\\',root)), intern =T )
# 

#building 'zBase_0.3.tar.gz'


browseURL('http://win-builder.r-project.org/upload.aspx')

	
	
}

