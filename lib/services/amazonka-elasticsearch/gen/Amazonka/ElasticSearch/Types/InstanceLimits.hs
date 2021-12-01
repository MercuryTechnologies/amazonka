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
-- Module      : Amazonka.ElasticSearch.Types.InstanceLimits
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElasticSearch.Types.InstanceLimits where

import qualified Amazonka.Core as Core
import Amazonka.ElasticSearch.Types.InstanceCountLimits
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | InstanceLimits represents the list of instance related attributes that
-- are available for given InstanceType.
--
-- /See:/ 'newInstanceLimits' smart constructor.
data InstanceLimits = InstanceLimits'
  { instanceCountLimits :: Prelude.Maybe InstanceCountLimits
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InstanceLimits' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceCountLimits', 'instanceLimits_instanceCountLimits' - Undocumented member.
newInstanceLimits ::
  InstanceLimits
newInstanceLimits =
  InstanceLimits'
    { instanceCountLimits =
        Prelude.Nothing
    }

-- | Undocumented member.
instanceLimits_instanceCountLimits :: Lens.Lens' InstanceLimits (Prelude.Maybe InstanceCountLimits)
instanceLimits_instanceCountLimits = Lens.lens (\InstanceLimits' {instanceCountLimits} -> instanceCountLimits) (\s@InstanceLimits' {} a -> s {instanceCountLimits = a} :: InstanceLimits)

instance Core.FromJSON InstanceLimits where
  parseJSON =
    Core.withObject
      "InstanceLimits"
      ( \x ->
          InstanceLimits'
            Prelude.<$> (x Core..:? "InstanceCountLimits")
      )

instance Prelude.Hashable InstanceLimits where
  hashWithSalt salt' InstanceLimits' {..} =
    salt' `Prelude.hashWithSalt` instanceCountLimits

instance Prelude.NFData InstanceLimits where
  rnf InstanceLimits' {..} =
    Prelude.rnf instanceCountLimits
