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
-- Module      : Amazonka.WorkMail.Types.Group
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WorkMail.Types.Group where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.WorkMail.Types.EntityState

-- | The representation of an Amazon WorkMail group.
--
-- /See:/ 'newGroup' smart constructor.
data Group = Group'
  { -- | The email of the group.
    email :: Prelude.Maybe Prelude.Text,
    -- | The state of the group, which can be ENABLED, DISABLED, or DELETED.
    state :: Prelude.Maybe EntityState,
    -- | The date indicating when the group was disabled from Amazon WorkMail
    -- use.
    disabledDate :: Prelude.Maybe Core.POSIX,
    -- | The name of the group.
    name :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the group.
    id :: Prelude.Maybe Prelude.Text,
    -- | The date indicating when the group was enabled for Amazon WorkMail use.
    enabledDate :: Prelude.Maybe Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Group' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'email', 'group_email' - The email of the group.
--
-- 'state', 'group_state' - The state of the group, which can be ENABLED, DISABLED, or DELETED.
--
-- 'disabledDate', 'group_disabledDate' - The date indicating when the group was disabled from Amazon WorkMail
-- use.
--
-- 'name', 'group_name' - The name of the group.
--
-- 'id', 'group_id' - The identifier of the group.
--
-- 'enabledDate', 'group_enabledDate' - The date indicating when the group was enabled for Amazon WorkMail use.
newGroup ::
  Group
newGroup =
  Group'
    { email = Prelude.Nothing,
      state = Prelude.Nothing,
      disabledDate = Prelude.Nothing,
      name = Prelude.Nothing,
      id = Prelude.Nothing,
      enabledDate = Prelude.Nothing
    }

-- | The email of the group.
group_email :: Lens.Lens' Group (Prelude.Maybe Prelude.Text)
group_email = Lens.lens (\Group' {email} -> email) (\s@Group' {} a -> s {email = a} :: Group)

-- | The state of the group, which can be ENABLED, DISABLED, or DELETED.
group_state :: Lens.Lens' Group (Prelude.Maybe EntityState)
group_state = Lens.lens (\Group' {state} -> state) (\s@Group' {} a -> s {state = a} :: Group)

-- | The date indicating when the group was disabled from Amazon WorkMail
-- use.
group_disabledDate :: Lens.Lens' Group (Prelude.Maybe Prelude.UTCTime)
group_disabledDate = Lens.lens (\Group' {disabledDate} -> disabledDate) (\s@Group' {} a -> s {disabledDate = a} :: Group) Prelude.. Lens.mapping Core._Time

-- | The name of the group.
group_name :: Lens.Lens' Group (Prelude.Maybe Prelude.Text)
group_name = Lens.lens (\Group' {name} -> name) (\s@Group' {} a -> s {name = a} :: Group)

-- | The identifier of the group.
group_id :: Lens.Lens' Group (Prelude.Maybe Prelude.Text)
group_id = Lens.lens (\Group' {id} -> id) (\s@Group' {} a -> s {id = a} :: Group)

-- | The date indicating when the group was enabled for Amazon WorkMail use.
group_enabledDate :: Lens.Lens' Group (Prelude.Maybe Prelude.UTCTime)
group_enabledDate = Lens.lens (\Group' {enabledDate} -> enabledDate) (\s@Group' {} a -> s {enabledDate = a} :: Group) Prelude.. Lens.mapping Core._Time

instance Core.FromJSON Group where
  parseJSON =
    Core.withObject
      "Group"
      ( \x ->
          Group'
            Prelude.<$> (x Core..:? "Email")
            Prelude.<*> (x Core..:? "State")
            Prelude.<*> (x Core..:? "DisabledDate")
            Prelude.<*> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "Id")
            Prelude.<*> (x Core..:? "EnabledDate")
      )

instance Prelude.Hashable Group where
  hashWithSalt salt' Group' {..} =
    salt' `Prelude.hashWithSalt` enabledDate
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` disabledDate
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` email

instance Prelude.NFData Group where
  rnf Group' {..} =
    Prelude.rnf email
      `Prelude.seq` Prelude.rnf enabledDate
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf disabledDate
      `Prelude.seq` Prelude.rnf state
