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
-- Module      : Amazonka.ElastiCache.Types.GlobalReplicationGroupInfo
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElastiCache.Types.GlobalReplicationGroupInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The name of the Global datastore and role of this replication group in
-- the Global datastore.
--
-- /See:/ 'newGlobalReplicationGroupInfo' smart constructor.
data GlobalReplicationGroupInfo = GlobalReplicationGroupInfo'
  { -- | The role of the replication group in a Global datastore. Can be primary
    -- or secondary.
    globalReplicationGroupMemberRole :: Prelude.Maybe Prelude.Text,
    -- | The name of the Global datastore
    globalReplicationGroupId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GlobalReplicationGroupInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'globalReplicationGroupMemberRole', 'globalReplicationGroupInfo_globalReplicationGroupMemberRole' - The role of the replication group in a Global datastore. Can be primary
-- or secondary.
--
-- 'globalReplicationGroupId', 'globalReplicationGroupInfo_globalReplicationGroupId' - The name of the Global datastore
newGlobalReplicationGroupInfo ::
  GlobalReplicationGroupInfo
newGlobalReplicationGroupInfo =
  GlobalReplicationGroupInfo'
    { globalReplicationGroupMemberRole =
        Prelude.Nothing,
      globalReplicationGroupId = Prelude.Nothing
    }

-- | The role of the replication group in a Global datastore. Can be primary
-- or secondary.
globalReplicationGroupInfo_globalReplicationGroupMemberRole :: Lens.Lens' GlobalReplicationGroupInfo (Prelude.Maybe Prelude.Text)
globalReplicationGroupInfo_globalReplicationGroupMemberRole = Lens.lens (\GlobalReplicationGroupInfo' {globalReplicationGroupMemberRole} -> globalReplicationGroupMemberRole) (\s@GlobalReplicationGroupInfo' {} a -> s {globalReplicationGroupMemberRole = a} :: GlobalReplicationGroupInfo)

-- | The name of the Global datastore
globalReplicationGroupInfo_globalReplicationGroupId :: Lens.Lens' GlobalReplicationGroupInfo (Prelude.Maybe Prelude.Text)
globalReplicationGroupInfo_globalReplicationGroupId = Lens.lens (\GlobalReplicationGroupInfo' {globalReplicationGroupId} -> globalReplicationGroupId) (\s@GlobalReplicationGroupInfo' {} a -> s {globalReplicationGroupId = a} :: GlobalReplicationGroupInfo)

instance Core.FromXML GlobalReplicationGroupInfo where
  parseXML x =
    GlobalReplicationGroupInfo'
      Prelude.<$> (x Core..@? "GlobalReplicationGroupMemberRole")
      Prelude.<*> (x Core..@? "GlobalReplicationGroupId")

instance Prelude.Hashable GlobalReplicationGroupInfo where
  hashWithSalt salt' GlobalReplicationGroupInfo' {..} =
    salt'
      `Prelude.hashWithSalt` globalReplicationGroupId
      `Prelude.hashWithSalt` globalReplicationGroupMemberRole

instance Prelude.NFData GlobalReplicationGroupInfo where
  rnf GlobalReplicationGroupInfo' {..} =
    Prelude.rnf globalReplicationGroupMemberRole
      `Prelude.seq` Prelude.rnf globalReplicationGroupId
