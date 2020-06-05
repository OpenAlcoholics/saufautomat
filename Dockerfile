
FROM mcr.microsoft.com/dotnet/core/sdk:3.1

RUN dotnet tool install -g Paket
RUN dotnet tool install -g fake-cli

# Workaround for https://github.com/dotnet/cli/issues/9321
ENV PATH="/root/.dotnet/tools:${PATH}"

# NodeJS 12.X
RUN curl -sL https://deb.nodesource.com/setup_12.x | bash
RUN apt-get install -y nodejs

WORKDIR /var/app

EXPOSE 8080

ADD .fable .fable
ADD public public
ADD src src
ADD Nuget.Config .
ADD package.json .
ADD package-lock.json .
ADD webpack.config.js .

RUN npm install
CMD npm start