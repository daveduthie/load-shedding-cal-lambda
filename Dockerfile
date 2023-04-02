FROM openjdk:latest
MAINTAINER Karol Wójcik <karol.wojcik@tuta.io>

ADD .holy-lambda/build/output.jar output.jar

CMD java -jar output.jar "io.github.daveduthie.load-shedding-cal-lambda.core.Lambda"
