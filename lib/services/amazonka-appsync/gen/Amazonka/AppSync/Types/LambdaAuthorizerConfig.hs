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
-- Module      : Amazonka.AppSync.Types.LambdaAuthorizerConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppSync.Types.LambdaAuthorizerConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | A @LambdaAuthorizerConfig@ holds configuration on how to authorize
-- AppSync API access when using the @AWS_LAMBDA@ authorizer mode. Be aware
-- that an AppSync API may have only one Lambda authorizer configured at a
-- time.
--
-- /See:/ 'newLambdaAuthorizerConfig' smart constructor.
data LambdaAuthorizerConfig = LambdaAuthorizerConfig'
  { -- | A regular expression for validation of tokens before the Lambda function
    -- is called.
    identityValidationExpression :: Prelude.Maybe Prelude.Text,
    -- | The number of seconds a response should be cached for. The default is 5
    -- minutes (300 seconds). The Lambda function can override this by
    -- returning a @ttlOverride@ key in its response. A value of 0 disables
    -- caching of responses.
    authorizerResultTtlInSeconds :: Prelude.Maybe Prelude.Natural,
    -- | The ARN of the Lambda function to be called for authorization. This may
    -- be a standard Lambda ARN, a version ARN (@...\/v3@) or alias ARN.
    --
    -- /Note/: This Lambda function must have the following resource-based
    -- policy assigned to it. When configuring Lambda authorizers in the
    -- Console, this is done for you. To do so with the Amazon Web Services
    -- CLI, run the following:
    --
    -- @aws lambda add-permission --function-name \"arn:aws:lambda:us-east-2:111122223333:function:my-function\" --statement-id \"appsync\" --principal appsync.amazonaws.com --action lambda:InvokeFunction@
    authorizerUri :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LambdaAuthorizerConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'identityValidationExpression', 'lambdaAuthorizerConfig_identityValidationExpression' - A regular expression for validation of tokens before the Lambda function
-- is called.
--
-- 'authorizerResultTtlInSeconds', 'lambdaAuthorizerConfig_authorizerResultTtlInSeconds' - The number of seconds a response should be cached for. The default is 5
-- minutes (300 seconds). The Lambda function can override this by
-- returning a @ttlOverride@ key in its response. A value of 0 disables
-- caching of responses.
--
-- 'authorizerUri', 'lambdaAuthorizerConfig_authorizerUri' - The ARN of the Lambda function to be called for authorization. This may
-- be a standard Lambda ARN, a version ARN (@...\/v3@) or alias ARN.
--
-- /Note/: This Lambda function must have the following resource-based
-- policy assigned to it. When configuring Lambda authorizers in the
-- Console, this is done for you. To do so with the Amazon Web Services
-- CLI, run the following:
--
-- @aws lambda add-permission --function-name \"arn:aws:lambda:us-east-2:111122223333:function:my-function\" --statement-id \"appsync\" --principal appsync.amazonaws.com --action lambda:InvokeFunction@
newLambdaAuthorizerConfig ::
  -- | 'authorizerUri'
  Prelude.Text ->
  LambdaAuthorizerConfig
newLambdaAuthorizerConfig pAuthorizerUri_ =
  LambdaAuthorizerConfig'
    { identityValidationExpression =
        Prelude.Nothing,
      authorizerResultTtlInSeconds = Prelude.Nothing,
      authorizerUri = pAuthorizerUri_
    }

-- | A regular expression for validation of tokens before the Lambda function
-- is called.
lambdaAuthorizerConfig_identityValidationExpression :: Lens.Lens' LambdaAuthorizerConfig (Prelude.Maybe Prelude.Text)
lambdaAuthorizerConfig_identityValidationExpression = Lens.lens (\LambdaAuthorizerConfig' {identityValidationExpression} -> identityValidationExpression) (\s@LambdaAuthorizerConfig' {} a -> s {identityValidationExpression = a} :: LambdaAuthorizerConfig)

-- | The number of seconds a response should be cached for. The default is 5
-- minutes (300 seconds). The Lambda function can override this by
-- returning a @ttlOverride@ key in its response. A value of 0 disables
-- caching of responses.
lambdaAuthorizerConfig_authorizerResultTtlInSeconds :: Lens.Lens' LambdaAuthorizerConfig (Prelude.Maybe Prelude.Natural)
lambdaAuthorizerConfig_authorizerResultTtlInSeconds = Lens.lens (\LambdaAuthorizerConfig' {authorizerResultTtlInSeconds} -> authorizerResultTtlInSeconds) (\s@LambdaAuthorizerConfig' {} a -> s {authorizerResultTtlInSeconds = a} :: LambdaAuthorizerConfig)

-- | The ARN of the Lambda function to be called for authorization. This may
-- be a standard Lambda ARN, a version ARN (@...\/v3@) or alias ARN.
--
-- /Note/: This Lambda function must have the following resource-based
-- policy assigned to it. When configuring Lambda authorizers in the
-- Console, this is done for you. To do so with the Amazon Web Services
-- CLI, run the following:
--
-- @aws lambda add-permission --function-name \"arn:aws:lambda:us-east-2:111122223333:function:my-function\" --statement-id \"appsync\" --principal appsync.amazonaws.com --action lambda:InvokeFunction@
lambdaAuthorizerConfig_authorizerUri :: Lens.Lens' LambdaAuthorizerConfig Prelude.Text
lambdaAuthorizerConfig_authorizerUri = Lens.lens (\LambdaAuthorizerConfig' {authorizerUri} -> authorizerUri) (\s@LambdaAuthorizerConfig' {} a -> s {authorizerUri = a} :: LambdaAuthorizerConfig)

instance Core.FromJSON LambdaAuthorizerConfig where
  parseJSON =
    Core.withObject
      "LambdaAuthorizerConfig"
      ( \x ->
          LambdaAuthorizerConfig'
            Prelude.<$> (x Core..:? "identityValidationExpression")
            Prelude.<*> (x Core..:? "authorizerResultTtlInSeconds")
            Prelude.<*> (x Core..: "authorizerUri")
      )

instance Prelude.Hashable LambdaAuthorizerConfig where
  hashWithSalt salt' LambdaAuthorizerConfig' {..} =
    salt' `Prelude.hashWithSalt` authorizerUri
      `Prelude.hashWithSalt` authorizerResultTtlInSeconds
      `Prelude.hashWithSalt` identityValidationExpression

instance Prelude.NFData LambdaAuthorizerConfig where
  rnf LambdaAuthorizerConfig' {..} =
    Prelude.rnf identityValidationExpression
      `Prelude.seq` Prelude.rnf authorizerUri
      `Prelude.seq` Prelude.rnf authorizerResultTtlInSeconds

instance Core.ToJSON LambdaAuthorizerConfig where
  toJSON LambdaAuthorizerConfig' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("identityValidationExpression" Core..=)
              Prelude.<$> identityValidationExpression,
            ("authorizerResultTtlInSeconds" Core..=)
              Prelude.<$> authorizerResultTtlInSeconds,
            Prelude.Just
              ("authorizerUri" Core..= authorizerUri)
          ]
      )
