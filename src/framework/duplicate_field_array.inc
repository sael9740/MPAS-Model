      threadNum = mpas_threading_get_thread_num()

      if ( threadNum == 0 ) then
         if (present(copy_array_only)) then
            local_copy_only = copy_array_only
         else
            local_copy_only = .false.
         end if

         
         src_cursor => src
         if (.not. local_copy_only) then
            nullify(dst_cursor)
         else
            dst_cursor => dst
         end if

!        do while (associated(src_cursor))

            if (.not. local_copy_only) then
               if (associated(dst_cursor)) then
                  allocate(dst_cursor % next)
                  dst_cursor % next % prev => dst_cursor
                  dst_cursor => dst_cursor % next
               else
                  allocate(dst)
                  nullify(dst % prev)
                  dst_cursor => dst
               end if
               nullify(dst_cursor % next)
            end if


            !
            ! Fill in members of dst_cursor from src_cursor
            !
            if (.not. local_copy_only) then
               dst_cursor % block => src_cursor % block
               dst_cursor % fieldName = src_cursor % fieldName
               dst_cursor % isVarArray = src_cursor % isVarArray
               if ( associated( src_cursor % constituentNames ) ) then
                  allocate(dst_cursor % constituentNames(size(src_cursor % constituentNames, dim=1)))
                  dst_cursor % constituentNames(:) = src_cursor % constituentNames(:)
               end if
               dst_cursor % isPersistent = src_cursor % isPersistent
               dst_cursor % isActive = src_cursor % isActive
               dst_cursor % isDecomposed = src_cursor % isDecomposed
               dst_cursor % hasTimeDimension = src_cursor % hasTimeDimension
               dst_cursor % dimNames = src_cursor % dimNames
               dst_cursor % dimSizes = src_cursor % dimSizes
               dst_cursor % sendList => src_cursor % sendList
               dst_cursor % recvList => src_cursor % recvList
               dst_cursor % copyList => src_cursor % copyList
               if ( dst_cursor % isActive ) then
                  call mpas_allocate_mold(dst_cursor % array, src_cursor % array)   ! Until we get F2008 support for ALLOCATE(A,MOLD=B)
               else
                  nullify(dst_cursor % array)
               end if

               if ( associated(dst_cursor % attLists) ) then
                  deallocate(dst_cursor % attLists)
               end if

               allocate(dst_cursor % attLists( size(src_cursor % attLists) ) )

               do iConstituent = 1, size(src_cursor % attLists)
                  call mpas_duplicate_attlist(src_cursor % attLists(iConstituent) % attList, dst_cursor % attLists(iConstituent) % attList)
               end do
            end if
            if ( dst_cursor % isActive .and. src_cursor % isActive ) then
               ! Copy on the device, so this is correct wherever the valid data lives.
               ! The cursor types are staged and their array members copied in explicitly: a
               ! pointer member used inside a compute construct needs its containing type
               ! present and the member attached, and the compiler's implicit data region
               ! silently reads host addresses once the arrays are resident. dst is created
               ! rather than copied in because the assignment overwrites it whole.
               !$acc enter data create(src_cursor, dst_cursor)
               !$acc enter data copyin(src_cursor % array)
               !$acc enter data create(dst_cursor % array)
               ! TODO revisit: nvfortran also emits a one-thread kernel here ("Accelerator
               ! serial kernel generated") to evaluate dst_cursor % array$sd after the loop,
               ! so every call costs a spare launch. The copy itself parallelises and
               ! collapses correctly, so this is sloppy rather than wrong, and there is
               ! probably a cleaner way to express the copy that avoids it. Naming the array
               ! section explicitly is not that way -- measured identical, and the shared .inc
               ! cannot express a per-rank section anyway.
               !$acc kernels
               dst_cursor % array = src_cursor % array
               !$acc end kernels
               ! These mirror the clauses above and restore the prior residency state exactly,
               ! because copyout and delete both decrement the reference count rather than
               ! acting unconditionally: not resident before, the result goes back to the host
               ! and the device copies are freed; already resident, the count simply returns to
               ! what it was. Leaving a reference behind would be silently destructive -- a
               ! later enter data copyin would find the field present and become a no-op.
               !$acc exit data copyout(dst_cursor % array)
               !$acc exit data delete(src_cursor % array)
               !$acc exit data delete(src_cursor, dst_cursor)
            end if

!           src_cursor => src_cursor % next
!           if (.not. local_copy_only) then
!              dst_cursor => dst_cursor % next
!           end if

!        end do
      end if
