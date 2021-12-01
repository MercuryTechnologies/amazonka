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
-- Module      : Amazonka.AppRunner.Types.AutoScalingConfigurationSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppRunner.Types.AutoScalingConfigurationSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Provides summary information about an App Runner automatic scaling
-- configuration resource.
--
-- This type contains limited information about an auto scaling
-- configuration. It includes only identification information, without
-- configuration details. It\'s returned by the
-- ListAutoScalingConfigurations action. Complete configuration information
-- is returned by the CreateAutoScalingConfiguration,
-- DescribeAutoScalingConfiguration, and DeleteAutoScalingConfiguration
-- actions using the AutoScalingConfiguration type.
--
-- /See:/ 'newAutoScalingConfigurationSummary' smart constructor.
data AutoScalingConfigurationSummary = AutoScalingConfigurationSummary'
  { -- | The customer-provided auto scaling configuration name. It can be used in
    -- multiple revisions of a configuration.
    autoScalingConfigurationName :: Prelude.Maybe Prelude.Text,
    -- | The revision of this auto scaling configuration. It\'s unique among all
    -- the active configurations (@\"Status\": \"ACTIVE\"@) with the same
    -- @AutoScalingConfigurationName@.
    autoScalingConfigurationRevision :: Prelude.Maybe Prelude.Int,
    -- | The Amazon Resource Name (ARN) of this auto scaling configuration.
    autoScalingConfigurationArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AutoScalingConfigurationSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'autoScalingConfigurationName', 'autoScalingConfigurationSummary_autoScalingConfigurationName' - The customer-provided auto scaling configuration name. It can be used in
-- multiple revisions of a configuration.
--
-- 'autoScalingConfigurationRevision', 'autoScalingConfigurationSummary_autoScalingConfigurationRevision' - The revision of this auto scaling configuration. It\'s unique among all
-- the active configurations (@\"Status\": \"ACTIVE\"@) with the same
-- @AutoScalingConfigurationName@.
--
-- 'autoScalingConfigurationArn', 'autoScalingConfigurationSummary_autoScalingConfigurationArn' - The Amazon Resource Name (ARN) of this auto scaling configuration.
newAutoScalingConfigurationSummary ::
  AutoScalingConfigurationSummary
newAutoScalingConfigurationSummary =
  AutoScalingConfigurationSummary'
    { autoScalingConfigurationName =
        Prelude.Nothing,
      autoScalingConfigurationRevision =
        Prelude.Nothing,
      autoScalingConfigurationArn =
        Prelude.Nothing
    }

-- | The customer-provided auto scaling configuration name. It can be used in
-- multiple revisions of a configuration.
autoScalingConfigurationSummary_autoScalingConfigurationName :: Lens.Lens' AutoScalingConfigurationSummary (Prelude.Maybe Prelude.Text)
autoScalingConfigurationSummary_autoScalingConfigurationName = Lens.lens (\AutoScalingConfigurationSummary' {autoScalingConfigurationName} -> autoScalingConfigurationName) (\s@AutoScalingConfigurationSummary' {} a -> s {autoScalingConfigurationName = a} :: AutoScalingConfigurationSummary)

-- | The revision of this auto scaling configuration. It\'s unique among all
-- the active configurations (@\"Status\": \"ACTIVE\"@) with the same
-- @AutoScalingConfigurationName@.
autoScalingConfigurationSummary_autoScalingConfigurationRevision :: Lens.Lens' AutoScalingConfigurationSummary (Prelude.Maybe Prelude.Int)
autoScalingConfigurationSummary_autoScalingConfigurationRevision = Lens.lens (\AutoScalingConfigurationSummary' {autoScalingConfigurationRevision} -> autoScalingConfigurationRevision) (\s@AutoScalingConfigurationSummary' {} a -> s {autoScalingConfigurationRevision = a} :: AutoScalingConfigurationSummary)

-- | The Amazon Resource Name (ARN) of this auto scaling configuration.
autoScalingConfigurationSummary_autoScalingConfigurationArn :: Lens.Lens' AutoScalingConfigurationSummary (Prelude.Maybe Prelude.Text)
autoScalingConfigurationSummary_autoScalingConfigurationArn = Lens.lens (\AutoScalingConfigurationSummary' {autoScalingConfigurationArn} -> autoScalingConfigurationArn) (\s@AutoScalingConfigurationSummary' {} a -> s {autoScalingConfigurationArn = a} :: AutoScalingConfigurationSummary)

instance
  Core.FromJSON
    AutoScalingConfigurationSummary
  where
  parseJSON =
    Core.withObject
      "AutoScalingConfigurationSummary"
      ( \x ->
          AutoScalingConfigurationSummary'
            Prelude.<$> (x Core..:? "AutoScalingConfigurationName")
            Prelude.<*> (x Core..:? "AutoScalingConfigurationRevision")
            Prelude.<*> (x Core..:? "AutoScalingConfigurationArn")
      )

instance
  Prelude.Hashable
    AutoScalingConfigurationSummary
  where
  hashWithSalt
    salt'
    AutoScalingConfigurationSummary' {..} =
      salt'
        `Prelude.hashWithSalt` autoScalingConfigurationArn
        `Prelude.hashWithSalt` autoScalingConfigurationRevision
        `Prelude.hashWithSalt` autoScalingConfigurationName

instance
  Prelude.NFData
    AutoScalingConfigurationSummary
  where
  rnf AutoScalingConfigurationSummary' {..} =
    Prelude.rnf autoScalingConfigurationName
      `Prelude.seq` Prelude.rnf autoScalingConfigurationArn
      `Prelude.seq` Prelude.rnf autoScalingConfigurationRevision
