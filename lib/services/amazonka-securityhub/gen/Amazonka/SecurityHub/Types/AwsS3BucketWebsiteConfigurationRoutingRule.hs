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
-- Module      : Amazonka.SecurityHub.Types.AwsS3BucketWebsiteConfigurationRoutingRule
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsS3BucketWebsiteConfigurationRoutingRule where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AwsS3BucketWebsiteConfigurationRoutingRuleCondition
import Amazonka.SecurityHub.Types.AwsS3BucketWebsiteConfigurationRoutingRuleRedirect

-- | A rule for redirecting requests to the website.
--
-- /See:/ 'newAwsS3BucketWebsiteConfigurationRoutingRule' smart constructor.
data AwsS3BucketWebsiteConfigurationRoutingRule = AwsS3BucketWebsiteConfigurationRoutingRule'
  { -- | Provides the rules to redirect the request if the condition in
    -- @Condition@ is met.
    redirect :: Prelude.Maybe AwsS3BucketWebsiteConfigurationRoutingRuleRedirect,
    -- | Provides the condition that must be met in order to apply the routing
    -- rule.
    condition :: Prelude.Maybe AwsS3BucketWebsiteConfigurationRoutingRuleCondition
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsS3BucketWebsiteConfigurationRoutingRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'redirect', 'awsS3BucketWebsiteConfigurationRoutingRule_redirect' - Provides the rules to redirect the request if the condition in
-- @Condition@ is met.
--
-- 'condition', 'awsS3BucketWebsiteConfigurationRoutingRule_condition' - Provides the condition that must be met in order to apply the routing
-- rule.
newAwsS3BucketWebsiteConfigurationRoutingRule ::
  AwsS3BucketWebsiteConfigurationRoutingRule
newAwsS3BucketWebsiteConfigurationRoutingRule =
  AwsS3BucketWebsiteConfigurationRoutingRule'
    { redirect =
        Prelude.Nothing,
      condition = Prelude.Nothing
    }

-- | Provides the rules to redirect the request if the condition in
-- @Condition@ is met.
awsS3BucketWebsiteConfigurationRoutingRule_redirect :: Lens.Lens' AwsS3BucketWebsiteConfigurationRoutingRule (Prelude.Maybe AwsS3BucketWebsiteConfigurationRoutingRuleRedirect)
awsS3BucketWebsiteConfigurationRoutingRule_redirect = Lens.lens (\AwsS3BucketWebsiteConfigurationRoutingRule' {redirect} -> redirect) (\s@AwsS3BucketWebsiteConfigurationRoutingRule' {} a -> s {redirect = a} :: AwsS3BucketWebsiteConfigurationRoutingRule)

-- | Provides the condition that must be met in order to apply the routing
-- rule.
awsS3BucketWebsiteConfigurationRoutingRule_condition :: Lens.Lens' AwsS3BucketWebsiteConfigurationRoutingRule (Prelude.Maybe AwsS3BucketWebsiteConfigurationRoutingRuleCondition)
awsS3BucketWebsiteConfigurationRoutingRule_condition = Lens.lens (\AwsS3BucketWebsiteConfigurationRoutingRule' {condition} -> condition) (\s@AwsS3BucketWebsiteConfigurationRoutingRule' {} a -> s {condition = a} :: AwsS3BucketWebsiteConfigurationRoutingRule)

instance
  Core.FromJSON
    AwsS3BucketWebsiteConfigurationRoutingRule
  where
  parseJSON =
    Core.withObject
      "AwsS3BucketWebsiteConfigurationRoutingRule"
      ( \x ->
          AwsS3BucketWebsiteConfigurationRoutingRule'
            Prelude.<$> (x Core..:? "Redirect")
              Prelude.<*> (x Core..:? "Condition")
      )

instance
  Prelude.Hashable
    AwsS3BucketWebsiteConfigurationRoutingRule
  where
  hashWithSalt
    salt'
    AwsS3BucketWebsiteConfigurationRoutingRule' {..} =
      salt' `Prelude.hashWithSalt` condition
        `Prelude.hashWithSalt` redirect

instance
  Prelude.NFData
    AwsS3BucketWebsiteConfigurationRoutingRule
  where
  rnf AwsS3BucketWebsiteConfigurationRoutingRule' {..} =
    Prelude.rnf redirect
      `Prelude.seq` Prelude.rnf condition

instance
  Core.ToJSON
    AwsS3BucketWebsiteConfigurationRoutingRule
  where
  toJSON
    AwsS3BucketWebsiteConfigurationRoutingRule' {..} =
      Core.object
        ( Prelude.catMaybes
            [ ("Redirect" Core..=) Prelude.<$> redirect,
              ("Condition" Core..=) Prelude.<$> condition
            ]
        )
