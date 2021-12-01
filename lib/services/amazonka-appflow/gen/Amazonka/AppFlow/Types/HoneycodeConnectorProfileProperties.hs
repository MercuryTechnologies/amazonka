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
-- Module      : Amazonka.AppFlow.Types.HoneycodeConnectorProfileProperties
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppFlow.Types.HoneycodeConnectorProfileProperties where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The connector-specific properties required when using Amazon Honeycode.
--
-- /See:/ 'newHoneycodeConnectorProfileProperties' smart constructor.
data HoneycodeConnectorProfileProperties = HoneycodeConnectorProfileProperties'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'HoneycodeConnectorProfileProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newHoneycodeConnectorProfileProperties ::
  HoneycodeConnectorProfileProperties
newHoneycodeConnectorProfileProperties =
  HoneycodeConnectorProfileProperties'

instance
  Core.FromJSON
    HoneycodeConnectorProfileProperties
  where
  parseJSON =
    Core.withObject
      "HoneycodeConnectorProfileProperties"
      ( \x ->
          Prelude.pure HoneycodeConnectorProfileProperties'
      )

instance
  Prelude.Hashable
    HoneycodeConnectorProfileProperties
  where
  hashWithSalt salt' _ =
    salt' `Prelude.hashWithSalt` (0 :: Prelude.Int)

instance
  Prelude.NFData
    HoneycodeConnectorProfileProperties
  where
  rnf _ = ()

instance
  Core.ToJSON
    HoneycodeConnectorProfileProperties
  where
  toJSON = Prelude.const (Core.Object Prelude.mempty)
