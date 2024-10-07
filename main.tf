terraform {

  backend "s3" {
    bucket = "js-interpreter-demo-terraform-state"
    key = "global/s3/terraform.tfstate"
    region = "us-east-1"
    dynamodb_table = "js-interpreter-state-lock"
    encrypt = true
  }

  required_providers {
    aws = {
      source  = "hashicorp/aws"
      version = "~> 5.48.0"
    }
    cloudflare = {
      source  = "cloudflare/cloudflare"
      version = "~> 4.0"
    }


  }

  required_version = ">= 1.2.0"
}

provider "aws" {
  region     = "us-east-1"
  access_key = var.AWS_ACCESS_KEY
  secret_key = var.AWS_SECRET_KEY
}

provider "cloudflare" {
        api_token = var.CLOUDFLARE_API_TOKEN
  }

variable "AWS_ACCESS_KEY" { type = string }

variable "AWS_SECRET_KEY" { type = string }

variable "CLOUDFLARE_API_TOKEN" { type = string }

variable "CLOUDFLARE_ZONE_ID" { type = string }

locals {
  content_type_map = {
   "js" = "text/javascript"
   "html" = "text/html"
   "css"  = "text/css"
   "ico" = "image/x-icon"
   "wasm" = "application/wasm"
  }
}


resource "aws_s3_bucket" "interpreter_demo_files" {
  bucket = "interpreter.dylanmashini.com"

  tags = {
    Name       = "JS Interpreter Demo"
    Enviorment = "Prod"
  }
}

resource "aws_s3_bucket_policy" "interpreter_demo_files" {
  bucket = aws_s3_bucket.interpreter_demo_files.id

  policy = jsonencode({
    Version = "2012-10-17"
    Statement = [
      {
        Action    = "s3:GetObject"
        Effect    = "Allow"
        Resource  = "arn:aws:s3:::${aws_s3_bucket.interpreter_demo_files.bucket}/*"
        Principal = "*"
      },
    ]
  })
  depends_on = [aws_s3_bucket_acl.interpreter_demo_files]
}

resource "aws_s3_bucket_public_access_block" "interpreter_demo_files" {
  bucket = aws_s3_bucket.interpreter_demo_files.id

  block_public_acls       = false
  block_public_policy     = false
  ignore_public_acls      = false
  restrict_public_buckets = false


}


resource "aws_s3_bucket_acl" "interpreter_demo_files" {
  depends_on = [
    aws_s3_bucket_ownership_controls.interpreter_demo_files,
    aws_s3_bucket_public_access_block.interpreter_demo_files,
  ]

  bucket = aws_s3_bucket.interpreter_demo_files.id
  acl    = "public-read"
}

resource "aws_s3_bucket_ownership_controls" "interpreter_demo_files" {
  bucket = aws_s3_bucket.interpreter_demo_files.id
  rule {
    object_ownership = "BucketOwnerPreferred"
  }
}




resource "aws_s3_bucket_cors_configuration" "interpreter_demo_files" {
  bucket = aws_s3_bucket.interpreter_demo_files.id
  cors_rule {
    allowed_origins = ["*"]
    allowed_methods = ["GET"]
  }
}

resource "aws_s3_object" "demo_files" {
  for_each     = fileset("./frontend/build", "**/*")
  bucket = aws_s3_bucket.interpreter_demo_files.id
  key = split(".html", each.value)[0]
    content_type = lookup(local.content_type_map, split(".", each.value)[length(split(".", each.value)) - 1], "text/html")
    etag = filemd5(format("%s%s", "./frontend/build/", each.value))
    source = format("%s%s", "./frontend/build/", each.value)
    depends_on   = [aws_s3_bucket_acl.interpreter_demo_files]
    acl = "public-read"
}

resource "aws_acm_certificate" "demo_certificate" {
  domain_name = "interpreter.dylanmashini.com"
  subject_alternative_names = ["interpreter.dylanmashini.com"]
  validation_method = "DNS"
  lifecycle {
    create_before_destroy = true
  }
}

resource "cloudflare_record" "demo_certificate_verification_record" {
    for_each = {
        for dvo in aws_acm_certificate.demo_certificate.domain_validation_options : dvo.domain_name => {
          name   = dvo.resource_record_name
          record = dvo.resource_record_value
          type   = dvo.resource_record_type
        }

    }
    zone_id = var.CLOUDFLARE_ZONE_ID
    name = each.value.name
    ttl = 1
    type = each.value.type
    proxied = false
    content = each.value.record
    comment = "Managed by Terraform"
}

locals {
  s3_origin_id = "staticInterpreterFiles"
}

resource "aws_acm_certificate_validation" "demo_certificate_verification" {
    certificate_arn = aws_acm_certificate.demo_certificate.arn
    depends_on = [cloudflare_record.demo_certificate_verification_record]
}

resource "aws_cloudfront_origin_access_control" "default" {
  name                              = "Default Interpreter S3 Access Control"
  origin_access_control_origin_type = "s3"
  signing_behavior                  = "always"
  signing_protocol                  = "sigv4"
}


resource "aws_cloudfront_distribution" "s3_files_distribution" {
    price_class = "PriceClass_100"

    aliases = ["interpreter.dylanmashini.com"]

    origin {
        domain_name = aws_s3_bucket.interpreter_demo_files.bucket_regional_domain_name
        origin_access_control_id =  aws_cloudfront_origin_access_control.default.id
        origin_id = local.s3_origin_id
    }
    enabled = true
    is_ipv6_enabled = true
    default_root_object = "index"

    default_cache_behavior {
        # Optimized Default
        cache_policy_id = "658327ea-f89d-4fab-a63d-7e88639e58f6"
        allowed_methods = ["GET", "HEAD", "OPTIONS"]
        cached_methods = ["GET", "HEAD"]
        target_origin_id = local.s3_origin_id
        viewer_protocol_policy = "redirect-to-https"
    }
    viewer_certificate {
        acm_certificate_arn = aws_acm_certificate_validation.demo_certificate_verification.certificate_arn
        ssl_support_method = "sni-only"
    }

    restrictions {
        geo_restriction {
            restriction_type = "none"
            locations = []
        }
    }
}

resource "cloudflare_record" "demo_dns_record" {
    name = "interpreter"
    type = "CNAME"
    zone_id = var.CLOUDFLARE_ZONE_ID
    content = aws_cloudfront_distribution.s3_files_distribution.domain_name
    comment = "Managed by Terraform"
}

