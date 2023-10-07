FROM mcr.microsoft.com/azure-functions/base:4-appservice 
ENV AzureWebJobsScriptRoot=/home/site/wwwroot \
    AzureFunctionsJobHost__Logging__Console__IsEnabled=true

COPY ./wwwroot /home/site/wwwroot
