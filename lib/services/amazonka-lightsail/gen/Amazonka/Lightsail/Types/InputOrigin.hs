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
-- Module      : Amazonka.Lightsail.Types.InputOrigin
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lightsail.Types.InputOrigin where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.Lightsail.Types.OriginProtocolPolicyEnum
import Amazonka.Lightsail.Types.RegionName
import qualified Amazonka.Prelude as Prelude

-- | Describes the origin resource of an Amazon Lightsail content delivery
-- network (CDN) distribution.
--
-- An origin can be a Lightsail instance or load balancer. A distribution
-- pulls content from an origin, caches it, and serves it to viewers via a
-- worldwide network of edge servers.
--
-- /See:/ 'newInputOrigin' smart constructor.
data InputOrigin = InputOrigin'
  { -- | The AWS Region name of the origin resource.
    regionName :: Prelude.Maybe RegionName,
    -- | The name of the origin resource.
    name :: Prelude.Maybe Prelude.Text,
    -- | The protocol that your Amazon Lightsail distribution uses when
    -- establishing a connection with your origin to pull content.
    protocolPolicy :: Prelude.Maybe OriginProtocolPolicyEnum
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InputOrigin' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'regionName', 'inputOrigin_regionName' - The AWS Region name of the origin resource.
--
-- 'name', 'inputOrigin_name' - The name of the origin resource.
--
-- 'protocolPolicy', 'inputOrigin_protocolPolicy' - The protocol that your Amazon Lightsail distribution uses when
-- establishing a connection with your origin to pull content.
newInputOrigin ::
  InputOrigin
newInputOrigin =
  InputOrigin'
    { regionName = Prelude.Nothing,
      name = Prelude.Nothing,
      protocolPolicy = Prelude.Nothing
    }

-- | The AWS Region name of the origin resource.
inputOrigin_regionName :: Lens.Lens' InputOrigin (Prelude.Maybe RegionName)
inputOrigin_regionName = Lens.lens (\InputOrigin' {regionName} -> regionName) (\s@InputOrigin' {} a -> s {regionName = a} :: InputOrigin)

-- | The name of the origin resource.
inputOrigin_name :: Lens.Lens' InputOrigin (Prelude.Maybe Prelude.Text)
inputOrigin_name = Lens.lens (\InputOrigin' {name} -> name) (\s@InputOrigin' {} a -> s {name = a} :: InputOrigin)

-- | The protocol that your Amazon Lightsail distribution uses when
-- establishing a connection with your origin to pull content.
inputOrigin_protocolPolicy :: Lens.Lens' InputOrigin (Prelude.Maybe OriginProtocolPolicyEnum)
inputOrigin_protocolPolicy = Lens.lens (\InputOrigin' {protocolPolicy} -> protocolPolicy) (\s@InputOrigin' {} a -> s {protocolPolicy = a} :: InputOrigin)

instance Prelude.Hashable InputOrigin where
  hashWithSalt salt' InputOrigin' {..} =
    salt' `Prelude.hashWithSalt` protocolPolicy
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` regionName

instance Prelude.NFData InputOrigin where
  rnf InputOrigin' {..} =
    Prelude.rnf regionName
      `Prelude.seq` Prelude.rnf protocolPolicy
      `Prelude.seq` Prelude.rnf name

instance Core.ToJSON InputOrigin where
  toJSON InputOrigin' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("regionName" Core..=) Prelude.<$> regionName,
            ("name" Core..=) Prelude.<$> name,
            ("protocolPolicy" Core..=)
              Prelude.<$> protocolPolicy
          ]
      )
