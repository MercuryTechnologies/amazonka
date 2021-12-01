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
-- Module      : Amazonka.IoTSiteWise.Types.GroupIdentity
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTSiteWise.Types.GroupIdentity where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Contains information for a group identity in an access policy.
--
-- /See:/ 'newGroupIdentity' smart constructor.
data GroupIdentity = GroupIdentity'
  { -- | The Amazon Web Services SSO ID of the group.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GroupIdentity' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'groupIdentity_id' - The Amazon Web Services SSO ID of the group.
newGroupIdentity ::
  -- | 'id'
  Prelude.Text ->
  GroupIdentity
newGroupIdentity pId_ = GroupIdentity' {id = pId_}

-- | The Amazon Web Services SSO ID of the group.
groupIdentity_id :: Lens.Lens' GroupIdentity Prelude.Text
groupIdentity_id = Lens.lens (\GroupIdentity' {id} -> id) (\s@GroupIdentity' {} a -> s {id = a} :: GroupIdentity)

instance Core.FromJSON GroupIdentity where
  parseJSON =
    Core.withObject
      "GroupIdentity"
      (\x -> GroupIdentity' Prelude.<$> (x Core..: "id"))

instance Prelude.Hashable GroupIdentity where
  hashWithSalt salt' GroupIdentity' {..} =
    salt' `Prelude.hashWithSalt` id

instance Prelude.NFData GroupIdentity where
  rnf GroupIdentity' {..} = Prelude.rnf id

instance Core.ToJSON GroupIdentity where
  toJSON GroupIdentity' {..} =
    Core.object
      (Prelude.catMaybes [Prelude.Just ("id" Core..= id)])
