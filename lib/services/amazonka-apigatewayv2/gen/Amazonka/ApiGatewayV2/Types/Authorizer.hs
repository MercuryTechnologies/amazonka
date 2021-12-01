{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.ApiGatewayV2.Types.Authorizer
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ApiGatewayV2.Types.Authorizer where

import Amazonka.ApiGatewayV2.Types.AuthorizerType
import Amazonka.ApiGatewayV2.Types.JWTConfiguration
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Represents an authorizer.
--
-- /See:/ 'newAuthorizer' smart constructor.
data Authorizer = Authorizer'
  { -- | Specifies the required credentials as an IAM role for API Gateway to
    -- invoke the authorizer. To specify an IAM role for API Gateway to assume,
    -- use the role\'s Amazon Resource Name (ARN). To use resource-based
    -- permissions on the Lambda function, don\'t specify this parameter.
    -- Supported only for REQUEST authorizers.
    authorizerCredentialsArn :: Prelude.Maybe Prelude.Text,
    -- | The validation expression does not apply to the REQUEST authorizer.
    identityValidationExpression :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether a Lambda authorizer returns a response in a simple
    -- format. If enabled, the Lambda authorizer can return a boolean value
    -- instead of an IAM policy. Supported only for HTTP APIs. To learn more,
    -- see
    -- <https://docs.aws.amazon.com/apigateway/latest/developerguide/http-api-lambda-authorizer.html Working with AWS Lambda authorizers for HTTP APIs>
    enableSimpleResponses :: Prelude.Maybe Prelude.Bool,
    -- | The authorizer\'s Uniform Resource Identifier (URI). For REQUEST
    -- authorizers, this must be a well-formed Lambda function URI, for
    -- example,
    -- arn:aws:apigateway:us-west-2:lambda:path\/2015-03-31\/functions\/arn:aws:lambda:us-west-2:{account_id}:function:{lambda_function_name}\/invocations.
    -- In general, the URI has this form:
    -- arn:aws:apigateway:{region}:lambda:path\/{service_api} , where {region}
    -- is the same as the region hosting the Lambda function, path indicates
    -- that the remaining substring in the URI should be treated as the path to
    -- the resource, including the initial \/. For Lambda functions, this is
    -- usually of the form \/2015-03-31\/functions\/[FunctionARN]\/invocations.
    -- Supported only for REQUEST authorizers.
    authorizerUri :: Prelude.Maybe Prelude.Text,
    -- | Specifies the format of the payload sent to an HTTP API Lambda
    -- authorizer. Required for HTTP API Lambda authorizers. Supported values
    -- are 1.0 and 2.0. To learn more, see
    -- <https://docs.aws.amazon.com/apigateway/latest/developerguide/http-api-lambda-authorizer.html Working with AWS Lambda authorizers for HTTP APIs>.
    authorizerPayloadFormatVersion :: Prelude.Maybe Prelude.Text,
    -- | Represents the configuration of a JWT authorizer. Required for the JWT
    -- authorizer type. Supported only for HTTP APIs.
    jwtConfiguration :: Prelude.Maybe JWTConfiguration,
    -- | The authorizer identifier.
    authorizerId :: Prelude.Maybe Prelude.Text,
    -- | The time to live (TTL) for cached authorizer results, in seconds. If it
    -- equals 0, authorization caching is disabled. If it is greater than 0,
    -- API Gateway caches authorizer responses. The maximum value is 3600, or 1
    -- hour. Supported only for HTTP API Lambda authorizers.
    authorizerResultTtlInSeconds :: Prelude.Maybe Prelude.Natural,
    -- | The identity source for which authorization is requested.
    --
    -- For a REQUEST authorizer, this is optional. The value is a set of one or
    -- more mapping expressions of the specified request parameters. The
    -- identity source can be headers, query string parameters, stage
    -- variables, and context parameters. For example, if an Auth header and a
    -- Name query string parameter are defined as identity sources, this value
    -- is route.request.header.Auth, route.request.querystring.Name for
    -- WebSocket APIs. For HTTP APIs, use selection expressions prefixed with
    -- \$, for example, $request.header.Auth, $request.querystring.Name. These
    -- parameters are used to perform runtime validation for Lambda-based
    -- authorizers by verifying all of the identity-related request parameters
    -- are present in the request, not null, and non-empty. Only when this is
    -- true does the authorizer invoke the authorizer Lambda function.
    -- Otherwise, it returns a 401 Unauthorized response without calling the
    -- Lambda function. For HTTP APIs, identity sources are also used as the
    -- cache key when caching is enabled. To learn more, see
    -- <https://docs.aws.amazon.com/apigateway/latest/developerguide/http-api-lambda-authorizer.html Working with AWS Lambda authorizers for HTTP APIs>.
    --
    -- For JWT, a single entry that specifies where to extract the JSON Web
    -- Token (JWT) from inbound requests. Currently only header-based and query
    -- parameter-based selections are supported, for example
    -- \$request.header.Authorization.
    identitySource :: Prelude.Maybe [Prelude.Text],
    -- | The authorizer type. Specify REQUEST for a Lambda function using
    -- incoming request parameters. Specify JWT to use JSON Web Tokens
    -- (supported only for HTTP APIs).
    authorizerType :: Prelude.Maybe AuthorizerType,
    -- | The name of the authorizer.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Authorizer' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'authorizerCredentialsArn', 'authorizer_authorizerCredentialsArn' - Specifies the required credentials as an IAM role for API Gateway to
-- invoke the authorizer. To specify an IAM role for API Gateway to assume,
-- use the role\'s Amazon Resource Name (ARN). To use resource-based
-- permissions on the Lambda function, don\'t specify this parameter.
-- Supported only for REQUEST authorizers.
--
-- 'identityValidationExpression', 'authorizer_identityValidationExpression' - The validation expression does not apply to the REQUEST authorizer.
--
-- 'enableSimpleResponses', 'authorizer_enableSimpleResponses' - Specifies whether a Lambda authorizer returns a response in a simple
-- format. If enabled, the Lambda authorizer can return a boolean value
-- instead of an IAM policy. Supported only for HTTP APIs. To learn more,
-- see
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/http-api-lambda-authorizer.html Working with AWS Lambda authorizers for HTTP APIs>
--
-- 'authorizerUri', 'authorizer_authorizerUri' - The authorizer\'s Uniform Resource Identifier (URI). For REQUEST
-- authorizers, this must be a well-formed Lambda function URI, for
-- example,
-- arn:aws:apigateway:us-west-2:lambda:path\/2015-03-31\/functions\/arn:aws:lambda:us-west-2:{account_id}:function:{lambda_function_name}\/invocations.
-- In general, the URI has this form:
-- arn:aws:apigateway:{region}:lambda:path\/{service_api} , where {region}
-- is the same as the region hosting the Lambda function, path indicates
-- that the remaining substring in the URI should be treated as the path to
-- the resource, including the initial \/. For Lambda functions, this is
-- usually of the form \/2015-03-31\/functions\/[FunctionARN]\/invocations.
-- Supported only for REQUEST authorizers.
--
-- 'authorizerPayloadFormatVersion', 'authorizer_authorizerPayloadFormatVersion' - Specifies the format of the payload sent to an HTTP API Lambda
-- authorizer. Required for HTTP API Lambda authorizers. Supported values
-- are 1.0 and 2.0. To learn more, see
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/http-api-lambda-authorizer.html Working with AWS Lambda authorizers for HTTP APIs>.
--
-- 'jwtConfiguration', 'authorizer_jwtConfiguration' - Represents the configuration of a JWT authorizer. Required for the JWT
-- authorizer type. Supported only for HTTP APIs.
--
-- 'authorizerId', 'authorizer_authorizerId' - The authorizer identifier.
--
-- 'authorizerResultTtlInSeconds', 'authorizer_authorizerResultTtlInSeconds' - The time to live (TTL) for cached authorizer results, in seconds. If it
-- equals 0, authorization caching is disabled. If it is greater than 0,
-- API Gateway caches authorizer responses. The maximum value is 3600, or 1
-- hour. Supported only for HTTP API Lambda authorizers.
--
-- 'identitySource', 'authorizer_identitySource' - The identity source for which authorization is requested.
--
-- For a REQUEST authorizer, this is optional. The value is a set of one or
-- more mapping expressions of the specified request parameters. The
-- identity source can be headers, query string parameters, stage
-- variables, and context parameters. For example, if an Auth header and a
-- Name query string parameter are defined as identity sources, this value
-- is route.request.header.Auth, route.request.querystring.Name for
-- WebSocket APIs. For HTTP APIs, use selection expressions prefixed with
-- \$, for example, $request.header.Auth, $request.querystring.Name. These
-- parameters are used to perform runtime validation for Lambda-based
-- authorizers by verifying all of the identity-related request parameters
-- are present in the request, not null, and non-empty. Only when this is
-- true does the authorizer invoke the authorizer Lambda function.
-- Otherwise, it returns a 401 Unauthorized response without calling the
-- Lambda function. For HTTP APIs, identity sources are also used as the
-- cache key when caching is enabled. To learn more, see
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/http-api-lambda-authorizer.html Working with AWS Lambda authorizers for HTTP APIs>.
--
-- For JWT, a single entry that specifies where to extract the JSON Web
-- Token (JWT) from inbound requests. Currently only header-based and query
-- parameter-based selections are supported, for example
-- \$request.header.Authorization.
--
-- 'authorizerType', 'authorizer_authorizerType' - The authorizer type. Specify REQUEST for a Lambda function using
-- incoming request parameters. Specify JWT to use JSON Web Tokens
-- (supported only for HTTP APIs).
--
-- 'name', 'authorizer_name' - The name of the authorizer.
newAuthorizer ::
  -- | 'name'
  Prelude.Text ->
  Authorizer
newAuthorizer pName_ =
  Authorizer'
    { authorizerCredentialsArn =
        Prelude.Nothing,
      identityValidationExpression = Prelude.Nothing,
      enableSimpleResponses = Prelude.Nothing,
      authorizerUri = Prelude.Nothing,
      authorizerPayloadFormatVersion = Prelude.Nothing,
      jwtConfiguration = Prelude.Nothing,
      authorizerId = Prelude.Nothing,
      authorizerResultTtlInSeconds = Prelude.Nothing,
      identitySource = Prelude.Nothing,
      authorizerType = Prelude.Nothing,
      name = pName_
    }

-- | Specifies the required credentials as an IAM role for API Gateway to
-- invoke the authorizer. To specify an IAM role for API Gateway to assume,
-- use the role\'s Amazon Resource Name (ARN). To use resource-based
-- permissions on the Lambda function, don\'t specify this parameter.
-- Supported only for REQUEST authorizers.
authorizer_authorizerCredentialsArn :: Lens.Lens' Authorizer (Prelude.Maybe Prelude.Text)
authorizer_authorizerCredentialsArn = Lens.lens (\Authorizer' {authorizerCredentialsArn} -> authorizerCredentialsArn) (\s@Authorizer' {} a -> s {authorizerCredentialsArn = a} :: Authorizer)

-- | The validation expression does not apply to the REQUEST authorizer.
authorizer_identityValidationExpression :: Lens.Lens' Authorizer (Prelude.Maybe Prelude.Text)
authorizer_identityValidationExpression = Lens.lens (\Authorizer' {identityValidationExpression} -> identityValidationExpression) (\s@Authorizer' {} a -> s {identityValidationExpression = a} :: Authorizer)

-- | Specifies whether a Lambda authorizer returns a response in a simple
-- format. If enabled, the Lambda authorizer can return a boolean value
-- instead of an IAM policy. Supported only for HTTP APIs. To learn more,
-- see
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/http-api-lambda-authorizer.html Working with AWS Lambda authorizers for HTTP APIs>
authorizer_enableSimpleResponses :: Lens.Lens' Authorizer (Prelude.Maybe Prelude.Bool)
authorizer_enableSimpleResponses = Lens.lens (\Authorizer' {enableSimpleResponses} -> enableSimpleResponses) (\s@Authorizer' {} a -> s {enableSimpleResponses = a} :: Authorizer)

-- | The authorizer\'s Uniform Resource Identifier (URI). For REQUEST
-- authorizers, this must be a well-formed Lambda function URI, for
-- example,
-- arn:aws:apigateway:us-west-2:lambda:path\/2015-03-31\/functions\/arn:aws:lambda:us-west-2:{account_id}:function:{lambda_function_name}\/invocations.
-- In general, the URI has this form:
-- arn:aws:apigateway:{region}:lambda:path\/{service_api} , where {region}
-- is the same as the region hosting the Lambda function, path indicates
-- that the remaining substring in the URI should be treated as the path to
-- the resource, including the initial \/. For Lambda functions, this is
-- usually of the form \/2015-03-31\/functions\/[FunctionARN]\/invocations.
-- Supported only for REQUEST authorizers.
authorizer_authorizerUri :: Lens.Lens' Authorizer (Prelude.Maybe Prelude.Text)
authorizer_authorizerUri = Lens.lens (\Authorizer' {authorizerUri} -> authorizerUri) (\s@Authorizer' {} a -> s {authorizerUri = a} :: Authorizer)

-- | Specifies the format of the payload sent to an HTTP API Lambda
-- authorizer. Required for HTTP API Lambda authorizers. Supported values
-- are 1.0 and 2.0. To learn more, see
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/http-api-lambda-authorizer.html Working with AWS Lambda authorizers for HTTP APIs>.
authorizer_authorizerPayloadFormatVersion :: Lens.Lens' Authorizer (Prelude.Maybe Prelude.Text)
authorizer_authorizerPayloadFormatVersion = Lens.lens (\Authorizer' {authorizerPayloadFormatVersion} -> authorizerPayloadFormatVersion) (\s@Authorizer' {} a -> s {authorizerPayloadFormatVersion = a} :: Authorizer)

-- | Represents the configuration of a JWT authorizer. Required for the JWT
-- authorizer type. Supported only for HTTP APIs.
authorizer_jwtConfiguration :: Lens.Lens' Authorizer (Prelude.Maybe JWTConfiguration)
authorizer_jwtConfiguration = Lens.lens (\Authorizer' {jwtConfiguration} -> jwtConfiguration) (\s@Authorizer' {} a -> s {jwtConfiguration = a} :: Authorizer)

-- | The authorizer identifier.
authorizer_authorizerId :: Lens.Lens' Authorizer (Prelude.Maybe Prelude.Text)
authorizer_authorizerId = Lens.lens (\Authorizer' {authorizerId} -> authorizerId) (\s@Authorizer' {} a -> s {authorizerId = a} :: Authorizer)

-- | The time to live (TTL) for cached authorizer results, in seconds. If it
-- equals 0, authorization caching is disabled. If it is greater than 0,
-- API Gateway caches authorizer responses. The maximum value is 3600, or 1
-- hour. Supported only for HTTP API Lambda authorizers.
authorizer_authorizerResultTtlInSeconds :: Lens.Lens' Authorizer (Prelude.Maybe Prelude.Natural)
authorizer_authorizerResultTtlInSeconds = Lens.lens (\Authorizer' {authorizerResultTtlInSeconds} -> authorizerResultTtlInSeconds) (\s@Authorizer' {} a -> s {authorizerResultTtlInSeconds = a} :: Authorizer)

-- | The identity source for which authorization is requested.
--
-- For a REQUEST authorizer, this is optional. The value is a set of one or
-- more mapping expressions of the specified request parameters. The
-- identity source can be headers, query string parameters, stage
-- variables, and context parameters. For example, if an Auth header and a
-- Name query string parameter are defined as identity sources, this value
-- is route.request.header.Auth, route.request.querystring.Name for
-- WebSocket APIs. For HTTP APIs, use selection expressions prefixed with
-- \$, for example, $request.header.Auth, $request.querystring.Name. These
-- parameters are used to perform runtime validation for Lambda-based
-- authorizers by verifying all of the identity-related request parameters
-- are present in the request, not null, and non-empty. Only when this is
-- true does the authorizer invoke the authorizer Lambda function.
-- Otherwise, it returns a 401 Unauthorized response without calling the
-- Lambda function. For HTTP APIs, identity sources are also used as the
-- cache key when caching is enabled. To learn more, see
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/http-api-lambda-authorizer.html Working with AWS Lambda authorizers for HTTP APIs>.
--
-- For JWT, a single entry that specifies where to extract the JSON Web
-- Token (JWT) from inbound requests. Currently only header-based and query
-- parameter-based selections are supported, for example
-- \$request.header.Authorization.
authorizer_identitySource :: Lens.Lens' Authorizer (Prelude.Maybe [Prelude.Text])
authorizer_identitySource = Lens.lens (\Authorizer' {identitySource} -> identitySource) (\s@Authorizer' {} a -> s {identitySource = a} :: Authorizer) Prelude.. Lens.mapping Lens.coerced

-- | The authorizer type. Specify REQUEST for a Lambda function using
-- incoming request parameters. Specify JWT to use JSON Web Tokens
-- (supported only for HTTP APIs).
authorizer_authorizerType :: Lens.Lens' Authorizer (Prelude.Maybe AuthorizerType)
authorizer_authorizerType = Lens.lens (\Authorizer' {authorizerType} -> authorizerType) (\s@Authorizer' {} a -> s {authorizerType = a} :: Authorizer)

-- | The name of the authorizer.
authorizer_name :: Lens.Lens' Authorizer Prelude.Text
authorizer_name = Lens.lens (\Authorizer' {name} -> name) (\s@Authorizer' {} a -> s {name = a} :: Authorizer)

instance Core.FromJSON Authorizer where
  parseJSON =
    Core.withObject
      "Authorizer"
      ( \x ->
          Authorizer'
            Prelude.<$> (x Core..:? "authorizerCredentialsArn")
            Prelude.<*> (x Core..:? "identityValidationExpression")
            Prelude.<*> (x Core..:? "enableSimpleResponses")
            Prelude.<*> (x Core..:? "authorizerUri")
            Prelude.<*> (x Core..:? "authorizerPayloadFormatVersion")
            Prelude.<*> (x Core..:? "jwtConfiguration")
            Prelude.<*> (x Core..:? "authorizerId")
            Prelude.<*> (x Core..:? "authorizerResultTtlInSeconds")
            Prelude.<*> (x Core..:? "identitySource" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "authorizerType")
            Prelude.<*> (x Core..: "name")
      )

instance Prelude.Hashable Authorizer where
  hashWithSalt salt' Authorizer' {..} =
    salt' `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` authorizerType
      `Prelude.hashWithSalt` identitySource
      `Prelude.hashWithSalt` authorizerResultTtlInSeconds
      `Prelude.hashWithSalt` authorizerId
      `Prelude.hashWithSalt` jwtConfiguration
      `Prelude.hashWithSalt` authorizerPayloadFormatVersion
      `Prelude.hashWithSalt` authorizerUri
      `Prelude.hashWithSalt` enableSimpleResponses
      `Prelude.hashWithSalt` identityValidationExpression
      `Prelude.hashWithSalt` authorizerCredentialsArn

instance Prelude.NFData Authorizer where
  rnf Authorizer' {..} =
    Prelude.rnf authorizerCredentialsArn
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf authorizerType
      `Prelude.seq` Prelude.rnf identitySource
      `Prelude.seq` Prelude.rnf authorizerResultTtlInSeconds
      `Prelude.seq` Prelude.rnf authorizerId
      `Prelude.seq` Prelude.rnf jwtConfiguration
      `Prelude.seq` Prelude.rnf authorizerPayloadFormatVersion
      `Prelude.seq` Prelude.rnf authorizerUri
      `Prelude.seq` Prelude.rnf enableSimpleResponses
      `Prelude.seq` Prelude.rnf identityValidationExpression
