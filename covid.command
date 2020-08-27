 # Must first install pandoc -- brew install pandoc
 # .command file needs permissions changed chmod +x covid.command
cd /Users/jamesbridget/Documents/covid_ma/
Rscript -e "rmarkdown::render('covid_figs.R', 'html_document', output_file='index.html', clean=T)"
