# Guide de Déploiement - Application Comptable Python

## 🚀 Vue d'ensemble du déploiement

Ce guide détaille les procédures de déploiement pour l'application comptable Python, conforme aux standards RNCP et aux exigences de production.

## 📋 Prérequis

### Infrastructure minimale
- **CPU** : 2 vCPUs (4 vCPUs en production)
- **RAM** : 4 GB (8 GB en production)
- **Stockage** : 50 GB SSD (100 GB en production)
- **Réseau** : Accès HTTPS, ports 80/443 ouverts

### Logiciels requis
```bash
# Sur le serveur de déploiement
Docker Engine 20.10+
Docker Compose 2.0+
Git 2.30+
OpenSSL 1.1.1+
```

## 🔧 Configuration initiale

### 1. Clonage du repository
```bash
# Cloner le projet
git clone https://github.com/votre-org/modernize-legacy-cobol-app-main.git
cd modernize-legacy-cobol-app-main

# Vérifier la version
git tag --list | tail -5
git checkout v1.0.0  # Version stable
```

### 2. Configuration des variables d'environnement
```bash
# Copier le template de configuration
cp .env.example .env

# Éditer avec vos valeurs de production
nano .env

# Vérifier la configuration
docker-compose config
```

### 3. Génération des certificats SSL
```bash
# Créer le dossier SSL
mkdir -p ssl

# Générer les certificats (Let's Encrypt recommandé)
certbot certonly --standalone \
  --email admin@votre-domaine.com \
  --agree-tos \
  --domains votre-domaine.com

# Copier les certificats
cp /etc/letsencrypt/live/votre-domaine.com/* ssl/
```

## 🐳 Déploiement avec Docker

### Déploiement standard
```bash
# Construction et démarrage
docker-compose up -d

# Vérification du statut
docker-compose ps
docker-compose logs accounting-app

# Tests de santé
curl http://localhost:8000/health
```

### Déploiement avec monitoring
```bash
# Avec services de monitoring
docker-compose -f docker-compose.yml -f docker-compose.monitoring.yml up -d

# Accès aux dashboards
echo "Application: http://localhost:8000"
echo "Monitoring: http://localhost:3000"
echo "Logs: http://localhost:5601"
```

## ☁️ Déploiement Cloud

### AWS ECS
```bash
# Configuration ECS
aws configure
aws ecs create-cluster --cluster-name accounting-app-cluster

# Déploiement de la task definition
aws ecs register-task-definition --cli-input-json file://aws-task-definition.json
```

### Azure Container Instances
```bash
# Connexion Azure
az login
az group create --name accounting-app-rg --location westeurope

# Déploiement
az container create \
  --resource-group accounting-app-rg \
  --name accounting-app \
  --image your-registry.azurecr.io/accounting-app:latest
```

### Google Cloud Run
```bash
# Configuration GCP
gcloud auth login
gcloud config set project your-project-id

# Déploiement
gcloud run deploy accounting-app \
  --image gcr.io/your-project/accounting-app:latest \
  --platform managed \
  --region europe-west1
```

## 🔄 Pipeline CI/CD

### GitHub Actions (inclus)
Le pipeline automatique inclut :
- Tests automatisés sur push
- Analyse de sécurité
- Construction d'image Docker
- Déploiement automatique

### GitLab CI/CD
```yaml
# .gitlab-ci.yml (optionnel)
stages:
  - test
  - security
  - build
  - deploy

test:
  stage: test
  script:
    - cd python-accounting-app && pytest

security:
  stage: security
  script:
    - bandit -r python-accounting-app/
    - safety check

deploy:
  stage: deploy
  script:
    - docker-compose up -d
  only:
    - main
```

## 📊 Monitoring et observabilité

### Métriques surveillées
```bash
# Métriques applicatives
- Temps de réponse des endpoints
- Nombre de transactions par minute
- Taux d'erreur
- Utilisation CPU/RAM

# Métriques infrastructure
- Disponibilité des services
- Utilisation disque
- Connectivité réseau
- Santé de la base de données
```

### Alertes configurées
```yaml
# Alertes Prometheus
groups:
  - name: accounting-app
    rules:
      - alert: HighResponseTime
        expr: response_time_seconds > 2
        for: 5m
        annotations:
          summary: "Temps de réponse élevé"
      
      - alert: HighErrorRate
        expr: error_rate > 0.05
        for: 2m
        annotations:
          summary: "Taux d'erreur élevé"
```

## 🔐 Sécurité en production

### Hardening du serveur
```bash
# Mise à jour système
sudo apt update && sudo apt upgrade -y

# Configuration firewall
sudo ufw enable
sudo ufw allow 22/tcp  # SSH
sudo ufw allow 80/tcp  # HTTP
sudo ufw allow 443/tcp # HTTPS

# Désactivation des services non nécessaires
sudo systemctl disable bluetooth
sudo systemctl disable cups
```

### Rotation des secrets
```bash
# Script de rotation automatique (cron mensuel)
#!/bin/bash
# rotate-secrets.sh

# Génération nouvelle clé JWT
NEW_JWT_KEY=$(openssl rand -base64 32)

# Mise à jour sécurisée
kubectl create secret generic app-secrets \
  --from-literal=jwt-key="$NEW_JWT_KEY" \
  --dry-run=client -o yaml | kubectl apply -f -

# Redémarrage rolling
kubectl rollout restart deployment/accounting-app
```

## 📝 Checklist de déploiement

### Pré-déploiement
- [ ] Tests passent en local
- [ ] Variables d'environnement configurées
- [ ] Certificats SSL valides
- [ ] Base de données migrée
- [ ] Sauvegardes récentes disponibles

### Déploiement
- [ ] Services démarrés sans erreur
- [ ] Health checks passent
- [ ] Logs normaux
- [ ] Tests de bout-en-bout réussis
- [ ] Monitoring opérationnel

### Post-déploiement
- [ ] Performance dans les SLA
- [ ] Aucune alerte critique
- [ ] Sauvegardes programmées
- [ ] Documentation mise à jour
- [ ] Équipe notifiée

## 🆘 Procédures de rollback

### Rollback automatique
```bash
# En cas d'échec des health checks
if ! curl -f http://localhost:8000/health; then
  echo "Health check failed, rolling back..."
  docker-compose down
  git checkout HEAD~1
  docker-compose up -d
fi
```

### Rollback manuel
```bash
# Retour à la version précédente
docker-compose down
git log --oneline -5  # Voir les commits récents
git checkout <commit-hash-stable>
docker-compose up -d

# Vérification
docker-compose logs accounting-app
curl http://localhost:8000/health
```

## 📞 Support et maintenance

### Équipe de support
- **DevOps** : devops@company.com
- **Développement** : dev@company.com  
- **Sécurité** : security@company.com

### Maintenance programmée
- **Quotidienne** : Vérification logs et métriques
- **Hebdomadaire** : Mise à jour sécurité
- **Mensuelle** : Optimisation performance
- **Trimestrielle** : Audit complet sécurité

### Documentation des incidents
Tous les incidents sont documentés dans notre système de ticketing avec :
- Description détaillée du problème
- Impact sur les utilisateurs
- Actions correctives prises
- Mesures préventives ajoutées
