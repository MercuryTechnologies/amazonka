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
-- Module      : Amazonka.MacieV2.Types.SecurityHubConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MacieV2.Types.SecurityHubConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Specifies configuration settings that determine which findings are
-- published to Security Hub automatically. For information about how Macie
-- publishes findings to Security Hub, see
-- <https://docs.aws.amazon.com/macie/latest/user/securityhub-integration.html Amazon Macie integration with Security Hub>
-- in the /Amazon Macie User Guide/.
--
-- /See:/ 'newSecurityHubConfiguration' smart constructor.
data SecurityHubConfiguration = SecurityHubConfiguration'
  { -- | Specifies whether to publish policy findings to Security Hub. If you set
    -- this value to true, Amazon Macie automatically publishes all new and
    -- updated policy findings that weren\'t suppressed by a findings filter.
    -- The default value is true.
    publishPolicyFindings :: Prelude.Bool,
    -- | Specifies whether to publish sensitive data findings to Security Hub. If
    -- you set this value to true, Amazon Macie automatically publishes all
    -- sensitive data findings that weren\'t suppressed by a findings filter.
    -- The default value is false.
    publishClassificationFindings :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SecurityHubConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'publishPolicyFindings', 'securityHubConfiguration_publishPolicyFindings' - Specifies whether to publish policy findings to Security Hub. If you set
-- this value to true, Amazon Macie automatically publishes all new and
-- updated policy findings that weren\'t suppressed by a findings filter.
-- The default value is true.
--
-- 'publishClassificationFindings', 'securityHubConfiguration_publishClassificationFindings' - Specifies whether to publish sensitive data findings to Security Hub. If
-- you set this value to true, Amazon Macie automatically publishes all
-- sensitive data findings that weren\'t suppressed by a findings filter.
-- The default value is false.
newSecurityHubConfiguration ::
  -- | 'publishPolicyFindings'
  Prelude.Bool ->
  -- | 'publishClassificationFindings'
  Prelude.Bool ->
  SecurityHubConfiguration
newSecurityHubConfiguration
  pPublishPolicyFindings_
  pPublishClassificationFindings_ =
    SecurityHubConfiguration'
      { publishPolicyFindings =
          pPublishPolicyFindings_,
        publishClassificationFindings =
          pPublishClassificationFindings_
      }

-- | Specifies whether to publish policy findings to Security Hub. If you set
-- this value to true, Amazon Macie automatically publishes all new and
-- updated policy findings that weren\'t suppressed by a findings filter.
-- The default value is true.
securityHubConfiguration_publishPolicyFindings :: Lens.Lens' SecurityHubConfiguration Prelude.Bool
securityHubConfiguration_publishPolicyFindings = Lens.lens (\SecurityHubConfiguration' {publishPolicyFindings} -> publishPolicyFindings) (\s@SecurityHubConfiguration' {} a -> s {publishPolicyFindings = a} :: SecurityHubConfiguration)

-- | Specifies whether to publish sensitive data findings to Security Hub. If
-- you set this value to true, Amazon Macie automatically publishes all
-- sensitive data findings that weren\'t suppressed by a findings filter.
-- The default value is false.
securityHubConfiguration_publishClassificationFindings :: Lens.Lens' SecurityHubConfiguration Prelude.Bool
securityHubConfiguration_publishClassificationFindings = Lens.lens (\SecurityHubConfiguration' {publishClassificationFindings} -> publishClassificationFindings) (\s@SecurityHubConfiguration' {} a -> s {publishClassificationFindings = a} :: SecurityHubConfiguration)

instance Core.FromJSON SecurityHubConfiguration where
  parseJSON =
    Core.withObject
      "SecurityHubConfiguration"
      ( \x ->
          SecurityHubConfiguration'
            Prelude.<$> (x Core..: "publishPolicyFindings")
            Prelude.<*> (x Core..: "publishClassificationFindings")
      )

instance Prelude.Hashable SecurityHubConfiguration where
  hashWithSalt salt' SecurityHubConfiguration' {..} =
    salt'
      `Prelude.hashWithSalt` publishClassificationFindings
      `Prelude.hashWithSalt` publishPolicyFindings

instance Prelude.NFData SecurityHubConfiguration where
  rnf SecurityHubConfiguration' {..} =
    Prelude.rnf publishPolicyFindings
      `Prelude.seq` Prelude.rnf publishClassificationFindings

instance Core.ToJSON SecurityHubConfiguration where
  toJSON SecurityHubConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "publishPolicyFindings"
                  Core..= publishPolicyFindings
              ),
            Prelude.Just
              ( "publishClassificationFindings"
                  Core..= publishClassificationFindings
              )
          ]
      )
