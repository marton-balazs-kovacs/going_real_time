# Going real-time

**The application is under developement. This is the first prototype version.**

This project explores possible solutions for the utilizitation of continuous data deposition, sharing, analysis and reporting to facilitate openness and credibility.

## Reporting results real-time

The application analyses google form responses real-time and create an interactive report from the results.

### Demo questionnaire

You can check out and answer the demo questionnaire on [HERE](https://forms.gle/Gg446ZPG3X2Eqea69).

### Using the app to report the results of your own questionnaire

#### Creating the google form

When you create your google form make sure that in the question text you write a short identifier first and separate it from the rest of the question with a dot.

For example, a question can look like this: *Q1. Why do I need an identifier?*

The app will use this identifiers as variable names that you can use in order to analyse the data or plot your results.

#### Link the responses to the app

The app reads in the data from a google spreadsheet that is directly linked to the google form.

* First, in order to report the results of your own questionnaire you have to set the access to the spreadsheet to "Public on the web". *(Please be aware, that after changing the access to public, anyone on the internet can find and view the responses.)*

* Second, you have to copy the url of the spreadsheet in the gs_url.txt file and save it.

* Third, you have to make sure that you have changed the variable names in the code in the app.R file to the variable names you used.

**Congratulations!**
You are done. You can deploy your application and see the automatically updated plots and results.

## Further plans

* Modularizing the app
* Transplant the app into a package
* Add interactive variable selection
* Add time machine to the BF plot
* Add robutness test to the BF analysis
* Make it possible to render pdf reports

## Auhors

* **Marton Kovacs**
* **Zoltan Kekecs**
