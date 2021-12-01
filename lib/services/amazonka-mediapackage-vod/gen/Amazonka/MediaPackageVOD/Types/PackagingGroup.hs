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
-- Module      : Amazonka.MediaPackageVOD.Types.PackagingGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaPackageVOD.Types.PackagingGroup where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.MediaPackageVOD.Types.Authorization
import Amazonka.MediaPackageVOD.Types.EgressAccessLogs
import qualified Amazonka.Prelude as Prelude

-- | A MediaPackage VOD PackagingGroup resource.
--
-- /See:/ 'newPackagingGroup' smart constructor.
data PackagingGroup = PackagingGroup'
  { -- | The ARN of the PackagingGroup.
    arn :: Prelude.Maybe Prelude.Text,
    authorization :: Prelude.Maybe Authorization,
    -- | The fully qualified domain name for Assets in the PackagingGroup.
    domainName :: Prelude.Maybe Prelude.Text,
    -- | The ID of the PackagingGroup.
    id :: Prelude.Maybe Prelude.Text,
    egressAccessLogs :: Prelude.Maybe EgressAccessLogs,
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PackagingGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'packagingGroup_arn' - The ARN of the PackagingGroup.
--
-- 'authorization', 'packagingGroup_authorization' - Undocumented member.
--
-- 'domainName', 'packagingGroup_domainName' - The fully qualified domain name for Assets in the PackagingGroup.
--
-- 'id', 'packagingGroup_id' - The ID of the PackagingGroup.
--
-- 'egressAccessLogs', 'packagingGroup_egressAccessLogs' - Undocumented member.
--
-- 'tags', 'packagingGroup_tags' - Undocumented member.
newPackagingGroup ::
  PackagingGroup
newPackagingGroup =
  PackagingGroup'
    { arn = Prelude.Nothing,
      authorization = Prelude.Nothing,
      domainName = Prelude.Nothing,
      id = Prelude.Nothing,
      egressAccessLogs = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | The ARN of the PackagingGroup.
packagingGroup_arn :: Lens.Lens' PackagingGroup (Prelude.Maybe Prelude.Text)
packagingGroup_arn = Lens.lens (\PackagingGroup' {arn} -> arn) (\s@PackagingGroup' {} a -> s {arn = a} :: PackagingGroup)

-- | Undocumented member.
packagingGroup_authorization :: Lens.Lens' PackagingGroup (Prelude.Maybe Authorization)
packagingGroup_authorization = Lens.lens (\PackagingGroup' {authorization} -> authorization) (\s@PackagingGroup' {} a -> s {authorization = a} :: PackagingGroup)

-- | The fully qualified domain name for Assets in the PackagingGroup.
packagingGroup_domainName :: Lens.Lens' PackagingGroup (Prelude.Maybe Prelude.Text)
packagingGroup_domainName = Lens.lens (\PackagingGroup' {domainName} -> domainName) (\s@PackagingGroup' {} a -> s {domainName = a} :: PackagingGroup)

-- | The ID of the PackagingGroup.
packagingGroup_id :: Lens.Lens' PackagingGroup (Prelude.Maybe Prelude.Text)
packagingGroup_id = Lens.lens (\PackagingGroup' {id} -> id) (\s@PackagingGroup' {} a -> s {id = a} :: PackagingGroup)

-- | Undocumented member.
packagingGroup_egressAccessLogs :: Lens.Lens' PackagingGroup (Prelude.Maybe EgressAccessLogs)
packagingGroup_egressAccessLogs = Lens.lens (\PackagingGroup' {egressAccessLogs} -> egressAccessLogs) (\s@PackagingGroup' {} a -> s {egressAccessLogs = a} :: PackagingGroup)

-- | Undocumented member.
packagingGroup_tags :: Lens.Lens' PackagingGroup (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
packagingGroup_tags = Lens.lens (\PackagingGroup' {tags} -> tags) (\s@PackagingGroup' {} a -> s {tags = a} :: PackagingGroup) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON PackagingGroup where
  parseJSON =
    Core.withObject
      "PackagingGroup"
      ( \x ->
          PackagingGroup'
            Prelude.<$> (x Core..:? "arn")
            Prelude.<*> (x Core..:? "authorization")
            Prelude.<*> (x Core..:? "domainName")
            Prelude.<*> (x Core..:? "id")
            Prelude.<*> (x Core..:? "egressAccessLogs")
            Prelude.<*> (x Core..:? "tags" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable PackagingGroup where
  hashWithSalt salt' PackagingGroup' {..} =
    salt' `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` egressAccessLogs
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` domainName
      `Prelude.hashWithSalt` authorization
      `Prelude.hashWithSalt` arn

instance Prelude.NFData PackagingGroup where
  rnf PackagingGroup' {..} =
    Prelude.rnf arn `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf egressAccessLogs
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf domainName
      `Prelude.seq` Prelude.rnf authorization
