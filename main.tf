pipeline {
    agent any

    environment {
        MAVEN_HOME = tool 'Maven 3.8.1'       // Adjust as per your Jenkins config
        DEPLOY_SERVER = 'user@your-tomcat-ip' // SSH user and host
        DEPLOY_PATH = '/opt/tomcat/webapps'   // Path to Tomcat webapps
        WAR_NAME = 'yourapp.war'              // WAR file name
    }

    stages {
        stage('Checkout') {
            steps {
                git 'https://github.com/your/repo.git'
            }
        }

        stage('Build with Maven') {
            steps {
                sh "${MAVEN_HOME}/bin/mvn clean package"
            }
        }

        stage('Deploy to Tomcat') {
            steps {deploy adapters: [tomcat9(credentialsId: 'tomcatpassword', path: '', url:'http://18.188.210.14:8080/')], contextPath: 'webapp', war: '**/*.war'

}
            }
        }
    }

    post {
        success {
            echo 'Deployed successfully!'
        }
        failure {
            echo 'Build or deploy failed.'
        }
    }
}
