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
-- Module      : Amazonka.LookoutMetrics.Types.AppFlowConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LookoutMetrics.Types.AppFlowConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Details about an Amazon AppFlow flow datasource.
--
-- /See:/ 'newAppFlowConfig' smart constructor.
data AppFlowConfig = AppFlowConfig'
  { -- | An IAM role that gives Amazon Lookout for Metrics permission to access
    -- the flow.
    roleArn :: Prelude.Text,
    -- | name of the flow.
    flowName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AppFlowConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'roleArn', 'appFlowConfig_roleArn' - An IAM role that gives Amazon Lookout for Metrics permission to access
-- the flow.
--
-- 'flowName', 'appFlowConfig_flowName' - name of the flow.
newAppFlowConfig ::
  -- | 'roleArn'
  Prelude.Text ->
  -- | 'flowName'
  Prelude.Text ->
  AppFlowConfig
newAppFlowConfig pRoleArn_ pFlowName_ =
  AppFlowConfig'
    { roleArn = pRoleArn_,
      flowName = pFlowName_
    }

-- | An IAM role that gives Amazon Lookout for Metrics permission to access
-- the flow.
appFlowConfig_roleArn :: Lens.Lens' AppFlowConfig Prelude.Text
appFlowConfig_roleArn = Lens.lens (\AppFlowConfig' {roleArn} -> roleArn) (\s@AppFlowConfig' {} a -> s {roleArn = a} :: AppFlowConfig)

-- | name of the flow.
appFlowConfig_flowName :: Lens.Lens' AppFlowConfig Prelude.Text
appFlowConfig_flowName = Lens.lens (\AppFlowConfig' {flowName} -> flowName) (\s@AppFlowConfig' {} a -> s {flowName = a} :: AppFlowConfig)

instance Core.FromJSON AppFlowConfig where
  parseJSON =
    Core.withObject
      "AppFlowConfig"
      ( \x ->
          AppFlowConfig'
            Prelude.<$> (x Core..: "RoleArn")
            Prelude.<*> (x Core..: "FlowName")
      )

instance Prelude.Hashable AppFlowConfig where
  hashWithSalt salt' AppFlowConfig' {..} =
    salt' `Prelude.hashWithSalt` flowName
      `Prelude.hashWithSalt` roleArn

instance Prelude.NFData AppFlowConfig where
  rnf AppFlowConfig' {..} =
    Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf flowName

instance Core.ToJSON AppFlowConfig where
  toJSON AppFlowConfig' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("RoleArn" Core..= roleArn),
            Prelude.Just ("FlowName" Core..= flowName)
          ]
      )
