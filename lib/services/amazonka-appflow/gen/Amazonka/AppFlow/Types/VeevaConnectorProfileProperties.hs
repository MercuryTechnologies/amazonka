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
-- Module      : Amazonka.AppFlow.Types.VeevaConnectorProfileProperties
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppFlow.Types.VeevaConnectorProfileProperties where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The connector-specific profile properties required when using Veeva.
--
-- /See:/ 'newVeevaConnectorProfileProperties' smart constructor.
data VeevaConnectorProfileProperties = VeevaConnectorProfileProperties'
  { -- | The location of the Veeva resource.
    instanceUrl :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VeevaConnectorProfileProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceUrl', 'veevaConnectorProfileProperties_instanceUrl' - The location of the Veeva resource.
newVeevaConnectorProfileProperties ::
  -- | 'instanceUrl'
  Prelude.Text ->
  VeevaConnectorProfileProperties
newVeevaConnectorProfileProperties pInstanceUrl_ =
  VeevaConnectorProfileProperties'
    { instanceUrl =
        pInstanceUrl_
    }

-- | The location of the Veeva resource.
veevaConnectorProfileProperties_instanceUrl :: Lens.Lens' VeevaConnectorProfileProperties Prelude.Text
veevaConnectorProfileProperties_instanceUrl = Lens.lens (\VeevaConnectorProfileProperties' {instanceUrl} -> instanceUrl) (\s@VeevaConnectorProfileProperties' {} a -> s {instanceUrl = a} :: VeevaConnectorProfileProperties)

instance
  Core.FromJSON
    VeevaConnectorProfileProperties
  where
  parseJSON =
    Core.withObject
      "VeevaConnectorProfileProperties"
      ( \x ->
          VeevaConnectorProfileProperties'
            Prelude.<$> (x Core..: "instanceUrl")
      )

instance
  Prelude.Hashable
    VeevaConnectorProfileProperties
  where
  hashWithSalt
    salt'
    VeevaConnectorProfileProperties' {..} =
      salt' `Prelude.hashWithSalt` instanceUrl

instance
  Prelude.NFData
    VeevaConnectorProfileProperties
  where
  rnf VeevaConnectorProfileProperties' {..} =
    Prelude.rnf instanceUrl

instance Core.ToJSON VeevaConnectorProfileProperties where
  toJSON VeevaConnectorProfileProperties' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("instanceUrl" Core..= instanceUrl)]
      )
