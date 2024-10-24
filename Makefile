build-shinylive:
	Rscript -e 'source("./assets/R/tools.R"); export_apps_json("./assets/R", "shinylive-app/shinylive");'

run-shinylive:
	Rscript -e 'httpuv::runStaticServer("shinylive-app");'