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
-- Module      : Amazonka.APIGateway.Types.Account
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.APIGateway.Types.Account where

import Amazonka.APIGateway.Types.ThrottleSettings
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Represents an AWS account that is associated with API Gateway.
--
-- To view the account info, call @GET@ on this resource.
--
-- ==== Error Codes
--
-- The following exception may be thrown when the request fails.
--
-- -   UnauthorizedException
-- -   NotFoundException
-- -   TooManyRequestsException
--
-- For detailed error code information, including the corresponding HTTP
-- Status Codes, see
-- <https://docs.aws.amazon.com/apigateway/api-reference/handling-errors/#api-error-codes API Gateway Error Codes>
--
-- ==== Example: Get the information about an account.
--
-- ===== Request
--
-- > GET /account HTTP/1.1 Content-Type: application/json Host: apigateway.us-east-1.amazonaws.com X-Amz-Date: 20160531T184618Z Authorization: AWS4-HMAC-SHA256 Credential={access_key_ID}/us-east-1/apigateway/aws4_request, SignedHeaders=content-type;host;x-amz-date, Signature={sig4_hash}
--
-- ===== Response
--
-- The successful response returns a @200 OK@ status code and a payload
-- similar to the following:
--
-- > { "_links": { "curies": { "href": "https://docs.aws.amazon.com/apigateway/latest/developerguide/account-apigateway-{rel}.html", "name": "account", "templated": true }, "self": { "href": "/account" }, "account:update": { "href": "/account" } }, "cloudwatchRoleArn": "arn:aws:iam::123456789012:role/apigAwsProxyRole", "throttleSettings": { "rateLimit": 500, "burstLimit": 1000 } }
--
-- In addition to making the REST API call directly, you can use the AWS
-- CLI and an AWS SDK to access this resource.
--
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/api-gateway-limits.html API Gateway Limits>
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/welcome.html Developer Guide>,
-- <https://docs.aws.amazon.com/cli/latest/reference/apigateway/get-account.html AWS CLI>
--
-- /See:/ 'newAccount' smart constructor.
data Account = Account'
  { -- | The version of the API keys used for the account.
    apiKeyVersion :: Prelude.Maybe Prelude.Text,
    -- | The ARN of an Amazon CloudWatch role for the current Account.
    cloudwatchRoleArn :: Prelude.Maybe Prelude.Text,
    -- | A list of features supported for the account. When usage plans are
    -- enabled, the features list will include an entry of @\"UsagePlans\"@.
    features :: Prelude.Maybe [Prelude.Text],
    -- | Specifies the API request limits configured for the current Account.
    throttleSettings :: Prelude.Maybe ThrottleSettings
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Account' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'apiKeyVersion', 'account_apiKeyVersion' - The version of the API keys used for the account.
--
-- 'cloudwatchRoleArn', 'account_cloudwatchRoleArn' - The ARN of an Amazon CloudWatch role for the current Account.
--
-- 'features', 'account_features' - A list of features supported for the account. When usage plans are
-- enabled, the features list will include an entry of @\"UsagePlans\"@.
--
-- 'throttleSettings', 'account_throttleSettings' - Specifies the API request limits configured for the current Account.
newAccount ::
  Account
newAccount =
  Account'
    { apiKeyVersion = Prelude.Nothing,
      cloudwatchRoleArn = Prelude.Nothing,
      features = Prelude.Nothing,
      throttleSettings = Prelude.Nothing
    }

-- | The version of the API keys used for the account.
account_apiKeyVersion :: Lens.Lens' Account (Prelude.Maybe Prelude.Text)
account_apiKeyVersion = Lens.lens (\Account' {apiKeyVersion} -> apiKeyVersion) (\s@Account' {} a -> s {apiKeyVersion = a} :: Account)

-- | The ARN of an Amazon CloudWatch role for the current Account.
account_cloudwatchRoleArn :: Lens.Lens' Account (Prelude.Maybe Prelude.Text)
account_cloudwatchRoleArn = Lens.lens (\Account' {cloudwatchRoleArn} -> cloudwatchRoleArn) (\s@Account' {} a -> s {cloudwatchRoleArn = a} :: Account)

-- | A list of features supported for the account. When usage plans are
-- enabled, the features list will include an entry of @\"UsagePlans\"@.
account_features :: Lens.Lens' Account (Prelude.Maybe [Prelude.Text])
account_features = Lens.lens (\Account' {features} -> features) (\s@Account' {} a -> s {features = a} :: Account) Prelude.. Lens.mapping Lens.coerced

-- | Specifies the API request limits configured for the current Account.
account_throttleSettings :: Lens.Lens' Account (Prelude.Maybe ThrottleSettings)
account_throttleSettings = Lens.lens (\Account' {throttleSettings} -> throttleSettings) (\s@Account' {} a -> s {throttleSettings = a} :: Account)

instance Core.FromJSON Account where
  parseJSON =
    Core.withObject
      "Account"
      ( \x ->
          Account'
            Prelude.<$> (x Core..:? "apiKeyVersion")
            Prelude.<*> (x Core..:? "cloudwatchRoleArn")
            Prelude.<*> (x Core..:? "features" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "throttleSettings")
      )

instance Prelude.Hashable Account where
  hashWithSalt salt' Account' {..} =
    salt' `Prelude.hashWithSalt` throttleSettings
      `Prelude.hashWithSalt` features
      `Prelude.hashWithSalt` cloudwatchRoleArn
      `Prelude.hashWithSalt` apiKeyVersion

instance Prelude.NFData Account where
  rnf Account' {..} =
    Prelude.rnf apiKeyVersion
      `Prelude.seq` Prelude.rnf throttleSettings
      `Prelude.seq` Prelude.rnf features
      `Prelude.seq` Prelude.rnf cloudwatchRoleArn
