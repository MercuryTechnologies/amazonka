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
-- Module      : Amazonka.WorkDocs.Types.UserMetadata
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WorkDocs.Types.UserMetadata where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes the metadata of the user.
--
-- /See:/ 'newUserMetadata' smart constructor.
data UserMetadata = UserMetadata'
  { -- | The given name of the user before a rename operation.
    givenName :: Prelude.Maybe Prelude.Text,
    -- | The name of the user.
    username :: Prelude.Maybe Prelude.Text,
    -- | The email address of the user.
    emailAddress :: Prelude.Maybe Prelude.Text,
    -- | The ID of the user.
    id :: Prelude.Maybe Prelude.Text,
    -- | The surname of the user.
    surname :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UserMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'givenName', 'userMetadata_givenName' - The given name of the user before a rename operation.
--
-- 'username', 'userMetadata_username' - The name of the user.
--
-- 'emailAddress', 'userMetadata_emailAddress' - The email address of the user.
--
-- 'id', 'userMetadata_id' - The ID of the user.
--
-- 'surname', 'userMetadata_surname' - The surname of the user.
newUserMetadata ::
  UserMetadata
newUserMetadata =
  UserMetadata'
    { givenName = Prelude.Nothing,
      username = Prelude.Nothing,
      emailAddress = Prelude.Nothing,
      id = Prelude.Nothing,
      surname = Prelude.Nothing
    }

-- | The given name of the user before a rename operation.
userMetadata_givenName :: Lens.Lens' UserMetadata (Prelude.Maybe Prelude.Text)
userMetadata_givenName = Lens.lens (\UserMetadata' {givenName} -> givenName) (\s@UserMetadata' {} a -> s {givenName = a} :: UserMetadata)

-- | The name of the user.
userMetadata_username :: Lens.Lens' UserMetadata (Prelude.Maybe Prelude.Text)
userMetadata_username = Lens.lens (\UserMetadata' {username} -> username) (\s@UserMetadata' {} a -> s {username = a} :: UserMetadata)

-- | The email address of the user.
userMetadata_emailAddress :: Lens.Lens' UserMetadata (Prelude.Maybe Prelude.Text)
userMetadata_emailAddress = Lens.lens (\UserMetadata' {emailAddress} -> emailAddress) (\s@UserMetadata' {} a -> s {emailAddress = a} :: UserMetadata)

-- | The ID of the user.
userMetadata_id :: Lens.Lens' UserMetadata (Prelude.Maybe Prelude.Text)
userMetadata_id = Lens.lens (\UserMetadata' {id} -> id) (\s@UserMetadata' {} a -> s {id = a} :: UserMetadata)

-- | The surname of the user.
userMetadata_surname :: Lens.Lens' UserMetadata (Prelude.Maybe Prelude.Text)
userMetadata_surname = Lens.lens (\UserMetadata' {surname} -> surname) (\s@UserMetadata' {} a -> s {surname = a} :: UserMetadata)

instance Core.FromJSON UserMetadata where
  parseJSON =
    Core.withObject
      "UserMetadata"
      ( \x ->
          UserMetadata'
            Prelude.<$> (x Core..:? "GivenName")
            Prelude.<*> (x Core..:? "Username")
            Prelude.<*> (x Core..:? "EmailAddress")
            Prelude.<*> (x Core..:? "Id")
            Prelude.<*> (x Core..:? "Surname")
      )

instance Prelude.Hashable UserMetadata where
  hashWithSalt salt' UserMetadata' {..} =
    salt' `Prelude.hashWithSalt` surname
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` emailAddress
      `Prelude.hashWithSalt` username
      `Prelude.hashWithSalt` givenName

instance Prelude.NFData UserMetadata where
  rnf UserMetadata' {..} =
    Prelude.rnf givenName
      `Prelude.seq` Prelude.rnf surname
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf emailAddress
      `Prelude.seq` Prelude.rnf username
