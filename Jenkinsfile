pipeline {
    agent any
    environment{
        DOCKER_REG="etd-docker01.it.csiro.au"
        DOCKER_IMAGE="gs1/supplychainserver"
        DOCKER_AUTH_CONFIG=credentials('DOCKER_AUTH_CONFIG')
    }
    stages {
        stage('Build') {
            steps {
                script {
                    docker.withRegistry("https://${DOCKER_REG}") {
                        checkout scm
                        
                        sh 'mkdir -p ~/.docker && echo $DOCKER_AUTH_CONFIG > ~/.docker/config.json'
                        
                        def latestImage = docker.build("${DOCKER_REG}/${DOCKER_IMAGE}", "-t ${DOCKER_REG}/${DOCKER_IMAGE}:${env.BUILD_ID} .")                        
                        latestImage.push()
                        
                        sh 'rm -rf ~/.docker'
                    }
                }
            }
        }
        stage('Test') {
            steps {
                script {
                    docker.image("${DOCKER_REG}/${DOCKER_IMAGE}").withRun('-p 8001:8000')  { c ->
                        sh 'ip=`netstat -nr | grep \'^0.0.0.0\' | awk \'{print $2}\'` && sleep 5 && curl -I $ip:8001/swagger.json'
                    }
                }
            }
        }
    }
}
